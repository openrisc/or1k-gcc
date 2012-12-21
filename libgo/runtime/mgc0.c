// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Garbage collector.

#include <unistd.h>

#include "runtime.h"
#include "arch.h"
#include "malloc.h"
#include "race.h"

#ifdef USING_SPLIT_STACK

extern void * __splitstack_find (void *, void *, size_t *, void **, void **,
				 void **);

extern void * __splitstack_find_context (void *context[10], size_t *, void **,
					 void **, void **);

#endif

enum {
	Debug = 0,
	DebugMark = 0,  // run second pass to check mark
	DataBlock = 8*1024,

	// Four bits per word (see #defines below).
	wordsPerBitmapWord = sizeof(void*)*8/4,
	bitShift = sizeof(void*)*8/4,
};

// Bits in per-word bitmap.
// #defines because enum might not be able to hold the values.
//
// Each word in the bitmap describes wordsPerBitmapWord words
// of heap memory.  There are 4 bitmap bits dedicated to each heap word,
// so on a 64-bit system there is one bitmap word per 16 heap words.
// The bits in the word are packed together by type first, then by
// heap location, so each 64-bit bitmap word consists of, from top to bottom,
// the 16 bitSpecial bits for the corresponding heap words, then the 16 bitMarked bits,
// then the 16 bitNoPointers/bitBlockBoundary bits, then the 16 bitAllocated bits.
// This layout makes it easier to iterate over the bits of a given type.
//
// The bitmap starts at mheap.arena_start and extends *backward* from
// there.  On a 64-bit system the off'th word in the arena is tracked by
// the off/16+1'th word before mheap.arena_start.  (On a 32-bit system,
// the only difference is that the divisor is 8.)
//
// To pull out the bits corresponding to a given pointer p, we use:
//
//	off = p - (uintptr*)mheap.arena_start;  // word offset
//	b = (uintptr*)mheap.arena_start - off/wordsPerBitmapWord - 1;
//	shift = off % wordsPerBitmapWord
//	bits = *b >> shift;
//	/* then test bits & bitAllocated, bits & bitMarked, etc. */
//
#define bitAllocated		((uintptr)1<<(bitShift*0))
#define bitNoPointers		((uintptr)1<<(bitShift*1))	/* when bitAllocated is set */
#define bitMarked		((uintptr)1<<(bitShift*2))	/* when bitAllocated is set */
#define bitSpecial		((uintptr)1<<(bitShift*3))	/* when bitAllocated is set - has finalizer or being profiled */
#define bitBlockBoundary	((uintptr)1<<(bitShift*1))	/* when bitAllocated is NOT set */

#define bitMask (bitBlockBoundary | bitAllocated | bitMarked | bitSpecial)

// Holding worldsema grants an M the right to try to stop the world.
// The procedure is:
//
//	runtime_semacquire(&runtime_worldsema);
//	m->gcing = 1;
//	runtime_stoptheworld();
//
//	... do stuff ...
//
//	m->gcing = 0;
//	runtime_semrelease(&runtime_worldsema);
//	runtime_starttheworld();
//
uint32 runtime_worldsema = 1;

static int32 gctrace;

typedef struct Workbuf Workbuf;
struct Workbuf
{
	LFNode node; // must be first
	uintptr nobj;
	byte *obj[512-(sizeof(LFNode)+sizeof(uintptr))/sizeof(byte*)];
};

typedef struct Finalizer Finalizer;
struct Finalizer
{
	void (*fn)(void*);
	void *arg;
	const struct __go_func_type *ft;
};

typedef struct FinBlock FinBlock;
struct FinBlock
{
	FinBlock *alllink;
	FinBlock *next;
	int32 cnt;
	int32 cap;
	Finalizer fin[1];
};

static G *fing;
static FinBlock *finq; // list of finalizers that are to be executed
static FinBlock *finc; // cache of free blocks
static FinBlock *allfin; // list of all blocks
static Lock finlock;
static int32 fingwait;

static void runfinq(void*);
static Workbuf* getempty(Workbuf*);
static Workbuf* getfull(Workbuf*);
static void	putempty(Workbuf*);
static Workbuf* handoff(Workbuf*);

typedef struct GcRoot GcRoot;
struct GcRoot
{
	byte *p;
	uintptr n;
};

static struct {
	uint64	full;  // lock-free list of full blocks
	uint64	empty; // lock-free list of empty blocks
	byte	pad0[CacheLineSize]; // prevents false-sharing between full/empty and nproc/nwait
	uint32	nproc;
	volatile uint32	nwait;
	volatile uint32	ndone;
	volatile uint32 debugmarkdone;
	Note	alldone;
	ParFor	*markfor;
	ParFor	*sweepfor;

	Lock;
	byte	*chunk;
	uintptr	nchunk;

	GcRoot	*roots;
	uint32	nroot;
	uint32	rootcap;
} work;

// scanblock scans a block of n bytes starting at pointer b for references
// to other objects, scanning any it finds recursively until there are no
// unscanned objects left.  Instead of using an explicit recursion, it keeps
// a work list in the Workbuf* structures and loops in the main function
// body.  Keeping an explicit work list is easier on the stack allocator and
// more efficient.
static void
scanblock(byte *b, uintptr n)
{
	byte *obj, *arena_start, *arena_used, *p;
	void **vp;
	uintptr size, *bitp, bits, shift, i, j, x, xbits, off, nobj, nproc;
	MSpan *s;
	PageID k;
	void **wp;
	Workbuf *wbuf;
	bool keepworking;

	if((intptr)n < 0) {
		runtime_printf("scanblock %p %D\n", b, (int64)n);
		runtime_throw("scanblock");
	}

	// Memory arena parameters.
	arena_start = runtime_mheap.arena_start;
	arena_used = runtime_mheap.arena_used;
	nproc = work.nproc;

	wbuf = nil;  // current work buffer
	wp = nil;  // storage for next queued pointer (write pointer)
	nobj = 0;  // number of queued objects

	// Scanblock helpers pass b==nil.
	// Procs needs to return to make more
	// calls to scanblock.  But if work.nproc==1 then
	// might as well process blocks as soon as we
	// have them.
	keepworking = b == nil || work.nproc == 1;

	// Align b to a word boundary.
	off = (uintptr)b & (PtrSize-1);
	if(off != 0) {
		b += PtrSize - off;
		n -= PtrSize - off;
	}

	for(;;) {
		// Each iteration scans the block b of length n, queueing pointers in
		// the work buffer.
		if(Debug > 1)
			runtime_printf("scanblock %p %D\n", b, (int64)n);

		vp = (void**)b;
		n >>= (2+PtrSize/8);  /* n /= PtrSize (4 or 8) */
		for(i=0; i<(uintptr)n; i++) {
			obj = (byte*)vp[i];

			// Words outside the arena cannot be pointers.
			if((byte*)obj < arena_start || (byte*)obj >= arena_used)
				continue;

			// obj may be a pointer to a live object.
			// Try to find the beginning of the object.

			// Round down to word boundary.
			obj = (void*)((uintptr)obj & ~((uintptr)PtrSize-1));

			// Find bits for this word.
			off = (uintptr*)obj - (uintptr*)arena_start;
			bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
			shift = off % wordsPerBitmapWord;
			xbits = *bitp;
			bits = xbits >> shift;

			// Pointing at the beginning of a block?
			if((bits & (bitAllocated|bitBlockBoundary)) != 0)
				goto found;

			// Pointing just past the beginning?
			// Scan backward a little to find a block boundary.
			for(j=shift; j-->0; ) {
				if(((xbits>>j) & (bitAllocated|bitBlockBoundary)) != 0) {
					obj = (byte*)obj - (shift-j)*PtrSize;
					shift = j;
					bits = xbits>>shift;
					goto found;
				}
			}

			// Otherwise consult span table to find beginning.
			// (Manually inlined copy of MHeap_LookupMaybe.)
			k = (uintptr)obj>>PageShift;
			x = k;
			if(sizeof(void*) == 8)
				x -= (uintptr)arena_start>>PageShift;
			s = runtime_mheap.map[x];
			if(s == nil || k < s->start || k - s->start >= s->npages || s->state != MSpanInUse)
				continue;
			p =  (byte*)((uintptr)s->start<<PageShift);
			if(s->sizeclass == 0) {
				obj = p;
			} else {
				if((byte*)obj >= (byte*)s->limit)
					continue;
				size = runtime_class_to_size[s->sizeclass];
				int32 i = ((byte*)obj - p)/size;
				obj = p+i*size;
			}

			// Now that we know the object header, reload bits.
			off = (uintptr*)obj - (uintptr*)arena_start;
			bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
			shift = off % wordsPerBitmapWord;
			xbits = *bitp;
			bits = xbits >> shift;

		found:
			// If another proc wants a pointer, give it some.
			if(work.nwait > 0 && nobj > 4 && work.full == 0) {
				wbuf->nobj = nobj;
				wbuf = handoff(wbuf);
				nobj = wbuf->nobj;
				wp = (void**)(wbuf->obj + nobj);
			}

			// Now we have bits, bitp, and shift correct for
			// obj pointing at the base of the object.
			// Only care about allocated and not marked.
			if((bits & (bitAllocated|bitMarked)) != bitAllocated)
				continue;
			if(nproc == 1)
				*bitp |= bitMarked<<shift;
			else {
				for(;;) {
					x = *bitp;
					if(x & (bitMarked<<shift))
						goto continue_obj;
					if(runtime_casp((void**)bitp, (void*)x, (void*)(x|(bitMarked<<shift))))
						break;
				}
			}

			// If object has no pointers, don't need to scan further.
			if((bits & bitNoPointers) != 0)
				continue;

			PREFETCH(obj);

			// If buffer is full, get a new one.
			if(wbuf == nil || nobj >= nelem(wbuf->obj)) {
				if(wbuf != nil)
					wbuf->nobj = nobj;
				wbuf = getempty(wbuf);
				wp = (void**)(wbuf->obj);
				nobj = 0;
			}
			*wp++ = obj;
			nobj++;
		continue_obj:;
		}

		// Done scanning [b, b+n).  Prepare for the next iteration of
		// the loop by setting b and n to the parameters for the next block.

		// Fetch b from the work buffer.
		if(nobj == 0) {
			if(!keepworking) {
				if(wbuf)
					putempty(wbuf);
				return;
			}
			// Emptied our buffer: refill.
			wbuf = getfull(wbuf);
			if(wbuf == nil)
				return;
			nobj = wbuf->nobj;
			wp = (void**)(wbuf->obj + wbuf->nobj);
		}
		b = *--wp;
		nobj--;

		// Ask span about size class.
		// (Manually inlined copy of MHeap_Lookup.)
		x = (uintptr)b>>PageShift;
		if(sizeof(void*) == 8)
			x -= (uintptr)arena_start>>PageShift;
		s = runtime_mheap.map[x];
		if(s->sizeclass == 0)
			n = s->npages<<PageShift;
		else
			n = runtime_class_to_size[s->sizeclass];
	}
}

// debug_scanblock is the debug copy of scanblock.
// it is simpler, slower, single-threaded, recursive,
// and uses bitSpecial as the mark bit.
static void
debug_scanblock(byte *b, uintptr n)
{
	byte *obj, *p;
	void **vp;
	uintptr size, *bitp, bits, shift, i, xbits, off;
	MSpan *s;

	if(!DebugMark)
		runtime_throw("debug_scanblock without DebugMark");

	if((intptr)n < 0) {
		runtime_printf("debug_scanblock %p %D\n", b, (int64)n);
		runtime_throw("debug_scanblock");
	}

	// Align b to a word boundary.
	off = (uintptr)b & (PtrSize-1);
	if(off != 0) {
		b += PtrSize - off;
		n -= PtrSize - off;
	}

	vp = (void**)b;
	n /= PtrSize;
	for(i=0; i<(uintptr)n; i++) {
		obj = (byte*)vp[i];

		// Words outside the arena cannot be pointers.
		if((byte*)obj < runtime_mheap.arena_start || (byte*)obj >= runtime_mheap.arena_used)
			continue;

		// Round down to word boundary.
		obj = (void*)((uintptr)obj & ~((uintptr)PtrSize-1));

		// Consult span table to find beginning.
		s = runtime_MHeap_LookupMaybe(&runtime_mheap, obj);
		if(s == nil)
			continue;

		p =  (byte*)((uintptr)s->start<<PageShift);
		if(s->sizeclass == 0) {
			obj = p;
			size = (uintptr)s->npages<<PageShift;
		} else {
			if((byte*)obj >= (byte*)s->limit)
				continue;
			size = runtime_class_to_size[s->sizeclass];
			int32 i = ((byte*)obj - p)/size;
			obj = p+i*size;
		}

		// Now that we know the object header, reload bits.
		off = (uintptr*)obj - (uintptr*)runtime_mheap.arena_start;
		bitp = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		xbits = *bitp;
		bits = xbits >> shift;

		// Now we have bits, bitp, and shift correct for
		// obj pointing at the base of the object.
		// If not allocated or already marked, done.
		if((bits & bitAllocated) == 0 || (bits & bitSpecial) != 0)  // NOTE: bitSpecial not bitMarked
			continue;
		*bitp |= bitSpecial<<shift;
		if(!(bits & bitMarked))
			runtime_printf("found unmarked block %p in %p\n", obj, vp+i);

		// If object has no pointers, don't need to scan further.
		if((bits & bitNoPointers) != 0)
			continue;

		debug_scanblock(obj, size);
	}
}

static void
markroot(ParFor *desc, uint32 i)
{
	USED(&desc);
	scanblock(work.roots[i].p, work.roots[i].n);
}

// Get an empty work buffer off the work.empty list,
// allocating new buffers as needed.
static Workbuf*
getempty(Workbuf *b)
{
	if(b != nil)
		runtime_lfstackpush(&work.full, &b->node);
	b = (Workbuf*)runtime_lfstackpop(&work.empty);
	if(b == nil) {
		// Need to allocate.
		runtime_lock(&work);
		if(work.nchunk < sizeof *b) {
			work.nchunk = 1<<20;
			work.chunk = runtime_SysAlloc(work.nchunk);
		}
		b = (Workbuf*)work.chunk;
		work.chunk += sizeof *b;
		work.nchunk -= sizeof *b;
		runtime_unlock(&work);
	}
	b->nobj = 0;
	return b;
}

static void
putempty(Workbuf *b)
{
	runtime_lfstackpush(&work.empty, &b->node);
}

// Get a full work buffer off the work.full list, or return nil.
static Workbuf*
getfull(Workbuf *b)
{
	M *m;
	int32 i;

	if(b != nil)
		runtime_lfstackpush(&work.empty, &b->node);
	b = (Workbuf*)runtime_lfstackpop(&work.full);
	if(b != nil || work.nproc == 1)
		return b;

	m = runtime_m();
	runtime_xadd(&work.nwait, +1);
	for(i=0;; i++) {
		if(work.full != 0) {
			runtime_xadd(&work.nwait, -1);
			b = (Workbuf*)runtime_lfstackpop(&work.full);
			if(b != nil)
				return b;
			runtime_xadd(&work.nwait, +1);
		}
		if(work.nwait == work.nproc)
			return nil;
		if(i < 10) {
			m->gcstats.nprocyield++;
			runtime_procyield(20);
		} else if(i < 20) {
			m->gcstats.nosyield++;
			runtime_osyield();
		} else {
			m->gcstats.nsleep++;
			runtime_usleep(100);
		}
	}
}

static Workbuf*
handoff(Workbuf *b)
{
	M *m;
	int32 n;
	Workbuf *b1;

	m = runtime_m();

	// Make new buffer with half of b's pointers.
	b1 = getempty(nil);
	n = b->nobj/2;
	b->nobj -= n;
	b1->nobj = n;
	runtime_memmove(b1->obj, b->obj+b->nobj, n*sizeof b1->obj[0]);
	m->gcstats.nhandoff++;
	m->gcstats.nhandoffcnt += n;

	// Put b on full list - let first half of b get stolen.
	runtime_lfstackpush(&work.full, &b->node);
	return b1;
}

static void
addroot(byte *p, uintptr n)
{
	uint32 cap;
	GcRoot *new;

	if(work.nroot >= work.rootcap) {
		cap = PageSize/sizeof(GcRoot);
		if(cap < 2*work.rootcap)
			cap = 2*work.rootcap;
		new = (GcRoot*)runtime_SysAlloc(cap*sizeof(GcRoot));
		if(work.roots != nil) {
			runtime_memmove(new, work.roots, work.rootcap*sizeof(GcRoot));
			runtime_SysFree(work.roots, work.rootcap*sizeof(GcRoot));
		}
		work.roots = new;
		work.rootcap = cap;
	}
	work.roots[work.nroot].p = p;
	work.roots[work.nroot].n = n;
	work.nroot++;
}

static void
addstackroots(G *gp)
{
#ifdef USING_SPLIT_STACK
	M *mp;
	void* sp;
	size_t spsize;
	void* next_segment;
	void* next_sp;
	void* initial_sp;

	if(gp == runtime_g()) {
		// Scanning our own stack.
		sp = __splitstack_find(nil, nil, &spsize, &next_segment,
				       &next_sp, &initial_sp);
	} else if((mp = gp->m) != nil && mp->helpgc) {
		// gchelper's stack is in active use and has no interesting pointers.
		return;
	} else {
		// Scanning another goroutine's stack.
		// The goroutine is usually asleep (the world is stopped).

		// The exception is that if the goroutine is about to enter or might
		// have just exited a system call, it may be executing code such
		// as schedlock and may have needed to start a new stack segment.
		// Use the stack segment and stack pointer at the time of
		// the system call instead, since that won't change underfoot.
		if(gp->gcstack != nil) {
			sp = gp->gcstack;
			spsize = gp->gcstack_size;
			next_segment = gp->gcnext_segment;
			next_sp = gp->gcnext_sp;
			initial_sp = gp->gcinitial_sp;
		} else {
			sp = __splitstack_find_context(&gp->stack_context[0],
						       &spsize, &next_segment,
						       &next_sp, &initial_sp);
		}
	}
	if(sp != nil) {
		addroot(sp, spsize);
		while((sp = __splitstack_find(next_segment, next_sp,
					      &spsize, &next_segment,
					      &next_sp, &initial_sp)) != nil)
			addroot(sp, spsize);
	}
#else
	M *mp;
	byte* bottom;
	byte* top;

	if(gp == runtime_g()) {
		// Scanning our own stack.
		bottom = (byte*)&gp;
	} else if((mp = gp->m) != nil && mp->helpgc) {
		// gchelper's stack is in active use and has no interesting pointers.
		return;
	} else {
		// Scanning another goroutine's stack.
		// The goroutine is usually asleep (the world is stopped).
		bottom = (byte*)gp->gcnext_sp;
		if(bottom == nil)
			return;
	}
	top = (byte*)gp->gcinitial_sp + gp->gcstack_size;
	if(top > bottom)
		addroot(bottom, top - bottom);
	else
		addroot(top, bottom - top);
#endif
}

static void
addfinroots(void *v)
{
	uintptr size;

	size = 0;
	if(!runtime_mlookup(v, (byte**)&v, &size, nil) || !runtime_blockspecial(v))
		runtime_throw("mark - finalizer inconsistency");

	// do not mark the finalizer block itself.  just mark the things it points at.
	addroot(v, size);
}

static struct root_list* roots;

void
__go_register_gc_roots (struct root_list* r)
{
	// FIXME: This needs locking if multiple goroutines can call
	// dlopen simultaneously.
	r->next = roots;
	roots = r;
}

static void
addroots(void)
{
	struct root_list *pl;
	G *gp;
	FinBlock *fb;
	MSpan *s, **allspans;
	uint32 spanidx;

	work.nroot = 0;

	// mark data+bss.
	for(pl = roots; pl != nil; pl = pl->next) {
		struct root* pr = &pl->roots[0];
		while(1) {
			void *decl = pr->decl;
			if(decl == nil)
				break;
			addroot(decl, pr->size);
			pr++;
		}
	}

	addroot((byte*)&runtime_m0, sizeof runtime_m0);
	addroot((byte*)&runtime_g0, sizeof runtime_g0);
	addroot((byte*)&runtime_allg, sizeof runtime_allg);
	addroot((byte*)&runtime_allm, sizeof runtime_allm);
	runtime_MProf_Mark(addroot);
	runtime_time_scan(addroot);
	runtime_trampoline_scan(addroot);

	// MSpan.types
	allspans = runtime_mheap.allspans;
	for(spanidx=0; spanidx<runtime_mheap.nspan; spanidx++) {
		s = allspans[spanidx];
		if(s->state == MSpanInUse) {
			switch(s->types.compression) {
			case MTypes_Empty:
			case MTypes_Single:
				break;
			case MTypes_Words:
			case MTypes_Bytes:
				addroot((byte*)&s->types.data, sizeof(void*));
				break;
			}
		}
	}

	for(gp=runtime_allg; gp!=nil; gp=gp->alllink) {
		switch(gp->status){
		default:
			runtime_printf("unexpected G.status %d\n", gp->status);
			runtime_throw("mark - bad status");
		case Gdead:
			break;
		case Grunning:
			if(gp != runtime_g())
				runtime_throw("mark - world not stopped");
			addstackroots(gp);
			break;
		case Grunnable:
		case Gsyscall:
		case Gwaiting:
			addstackroots(gp);
			break;
		}
	}

	runtime_walkfintab(addfinroots, addroot);

	for(fb=allfin; fb; fb=fb->alllink)
		addroot((byte*)fb->fin, fb->cnt*sizeof(fb->fin[0]));

	addroot((byte*)&work, sizeof work);
}

static bool
handlespecial(byte *p, uintptr size)
{
	void (*fn)(void*);
	const struct __go_func_type *ft;
	FinBlock *block;
	Finalizer *f;
	
	if(!runtime_getfinalizer(p, true, &fn, &ft)) {
		runtime_setblockspecial(p, false);
		runtime_MProf_Free(p, size);
		return false;
	}

	runtime_lock(&finlock);
	if(finq == nil || finq->cnt == finq->cap) {
		if(finc == nil) {
			finc = runtime_SysAlloc(PageSize);
			finc->cap = (PageSize - sizeof(FinBlock)) / sizeof(Finalizer) + 1;
			finc->alllink = allfin;
			allfin = finc;
		}
		block = finc;
		finc = block->next;
		block->next = finq;
		finq = block;
	}
	f = &finq->fin[finq->cnt];
	finq->cnt++;
	f->fn = fn;
	f->ft = ft;
	f->arg = p;
	runtime_unlock(&finlock);
	return true;
}

// Sweep frees or collects finalizers for blocks not marked in the mark phase.
// It clears the mark bits in preparation for the next GC round.
static void
sweepspan(ParFor *desc, uint32 idx)
{
	M *m;
	int32 cl, n, npages;
	uintptr size;
	byte *p;
	MCache *c;
	byte *arena_start;
	MLink head, *end;
	int32 nfree;
	byte *type_data;
	byte compression;
	uintptr type_data_inc;
	MSpan *s;

	m = runtime_m();

	USED(&desc);
	s = runtime_mheap.allspans[idx];
	// Stamp newly unused spans. The scavenger will use that
	// info to potentially give back some pages to the OS.
	if(s->state == MSpanFree && s->unusedsince == 0)
		s->unusedsince = runtime_nanotime();
	if(s->state != MSpanInUse)
		return;
	arena_start = runtime_mheap.arena_start;
	p = (byte*)(s->start << PageShift);
	cl = s->sizeclass;
	size = s->elemsize;
	if(cl == 0) {
		n = 1;
	} else {
		// Chunk full of small blocks.
		npages = runtime_class_to_allocnpages[cl];
		n = (npages << PageShift) / size;
	}
	nfree = 0;
	end = &head;
	c = m->mcache;
	
	type_data = (byte*)s->types.data;
	type_data_inc = sizeof(uintptr);
	compression = s->types.compression;
	switch(compression) {
	case MTypes_Bytes:
		type_data += 8*sizeof(uintptr);
		type_data_inc = 1;
		break;
	}

	// Sweep through n objects of given size starting at p.
	// This thread owns the span now, so it can manipulate
	// the block bitmap without atomic operations.
	for(; n > 0; n--, p += size, type_data+=type_data_inc) {
		uintptr off, *bitp, shift, bits;

		off = (uintptr*)p - (uintptr*)arena_start;
		bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		bits = *bitp>>shift;

		if((bits & bitAllocated) == 0)
			continue;

		if((bits & bitMarked) != 0) {
			if(DebugMark) {
				if(!(bits & bitSpecial))
					runtime_printf("found spurious mark on %p\n", p);
				*bitp &= ~(bitSpecial<<shift);
			}
			*bitp &= ~(bitMarked<<shift);
			continue;
		}

		// Special means it has a finalizer or is being profiled.
		// In DebugMark mode, the bit has been coopted so
		// we have to assume all blocks are special.
		if(DebugMark || (bits & bitSpecial) != 0) {
			if(handlespecial(p, size))
				continue;
		}

		// Mark freed; restore block boundary bit.
		*bitp = (*bitp & ~(bitMask<<shift)) | (bitBlockBoundary<<shift);

		if(cl == 0) {
			// Free large span.
			runtime_unmarkspan(p, 1<<PageShift);
			*(uintptr*)p = 1;	// needs zeroing
			runtime_MHeap_Free(&runtime_mheap, s, 1);
			c->local_alloc -= size;
			c->local_nfree++;
		} else {
			// Free small object.
			switch(compression) {
			case MTypes_Words:
				*(uintptr*)type_data = 0;
				break;
			case MTypes_Bytes:
				*(byte*)type_data = 0;
				break;
			}
			if(size > sizeof(uintptr))
				((uintptr*)p)[1] = 1;	// mark as "needs to be zeroed"
			
			end->next = (MLink*)p;
			end = (MLink*)p;
			nfree++;
		}
	}

	if(nfree) {
		c->local_by_size[cl].nfree += nfree;
		c->local_alloc -= size * nfree;
		c->local_nfree += nfree;
		c->local_cachealloc -= nfree * size;
		c->local_objects -= nfree;
		runtime_MCentral_FreeSpan(&runtime_mheap.central[cl], s, nfree, head.next, end);
	}
}

static void
dumpspan(uint32 idx)
{
	int32 sizeclass, n, npages, i, column;
	uintptr size;
	byte *p;
	byte *arena_start;
	MSpan *s;
	bool allocated, special;

	s = runtime_mheap.allspans[idx];
	if(s->state != MSpanInUse)
		return;
	arena_start = runtime_mheap.arena_start;
	p = (byte*)(s->start << PageShift);
	sizeclass = s->sizeclass;
	size = s->elemsize;
	if(sizeclass == 0) {
		n = 1;
	} else {
		npages = runtime_class_to_allocnpages[sizeclass];
		n = (npages << PageShift) / size;
	}
	
	runtime_printf("%p .. %p:\n", p, p+n*size);
	column = 0;
	for(; n>0; n--, p+=size) {
		uintptr off, *bitp, shift, bits;

		off = (uintptr*)p - (uintptr*)arena_start;
		bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		bits = *bitp>>shift;

		allocated = ((bits & bitAllocated) != 0);
		special = ((bits & bitSpecial) != 0);

		for(i=0; (uint32)i<size; i+=sizeof(void*)) {
			if(column == 0) {
				runtime_printf("\t");
			}
			if(i == 0) {
				runtime_printf(allocated ? "(" : "[");
				runtime_printf(special ? "@" : "");
				runtime_printf("%p: ", p+i);
			} else {
				runtime_printf(" ");
			}

			runtime_printf("%p", *(void**)(p+i));

			if(i+sizeof(void*) >= size) {
				runtime_printf(allocated ? ") " : "] ");
			}

			column++;
			if(column == 8) {
				runtime_printf("\n");
				column = 0;
			}
		}
	}
	runtime_printf("\n");
}

// A debugging function to dump the contents of memory
void
runtime_memorydump(void)
{
	uint32 spanidx;

	for(spanidx=0; spanidx<runtime_mheap.nspan; spanidx++) {
		dumpspan(spanidx);
	}
}
void
runtime_gchelper(void)
{
	// parallel mark for over gc roots
	runtime_parfordo(work.markfor);
	// help other threads scan secondary blocks
	scanblock(nil, 0);

	if(DebugMark) {
		// wait while the main thread executes mark(debug_scanblock)
		while(runtime_atomicload(&work.debugmarkdone) == 0)
			runtime_usleep(10);
	}

	runtime_parfordo(work.sweepfor);
	if(runtime_xadd(&work.ndone, +1) == work.nproc-1)
		runtime_notewakeup(&work.alldone);
}

// Initialized from $GOGC.  GOGC=off means no gc.
//
// Next gc is after we've allocated an extra amount of
// memory proportional to the amount already in use.
// If gcpercent=100 and we're using 4M, we'll gc again
// when we get to 8M.  This keeps the gc cost in linear
// proportion to the allocation cost.  Adjusting gcpercent
// just changes the linear constant (and also the amount of
// extra memory used).
static int32 gcpercent = -2;

static void
stealcache(void)
{
	M *m;

	for(m=runtime_allm; m; m=m->alllink)
		runtime_MCache_ReleaseAll(m->mcache);
}

static void
cachestats(GCStats *stats)
{
	M *m;
	MCache *c;
	uint32 i;
	uint64 stacks_inuse;
	uint64 stacks_sys;
	uint64 *src, *dst;

	if(stats)
		runtime_memclr((byte*)stats, sizeof(*stats));
	stacks_inuse = 0;
	stacks_sys = runtime_stacks_sys;
	for(m=runtime_allm; m; m=m->alllink) {
		c = m->mcache;
		runtime_purgecachedstats(c);
		// stacks_inuse += m->stackalloc->inuse;
		// stacks_sys += m->stackalloc->sys;
		if(stats) {
			src = (uint64*)&m->gcstats;
			dst = (uint64*)stats;
			for(i=0; i<sizeof(*stats)/sizeof(uint64); i++)
				dst[i] += src[i];
			runtime_memclr((byte*)&m->gcstats, sizeof(m->gcstats));
		}
		for(i=0; i<nelem(c->local_by_size); i++) {
			mstats.by_size[i].nmalloc += c->local_by_size[i].nmalloc;
			c->local_by_size[i].nmalloc = 0;
			mstats.by_size[i].nfree += c->local_by_size[i].nfree;
			c->local_by_size[i].nfree = 0;
		}
	}
	mstats.stacks_inuse = stacks_inuse;
	mstats.stacks_sys = stacks_sys;
}

void
runtime_gc(int32 force)
{
	M *m;
	int64 t0, t1, t2, t3;
	uint64 heap0, heap1, obj0, obj1;
	const byte *p;
	GCStats stats;
	M *m1;
	uint32 i;

	// The atomic operations are not atomic if the uint64s
	// are not aligned on uint64 boundaries. This has been
	// a problem in the past.
	if((((uintptr)&work.empty) & 7) != 0)
		runtime_throw("runtime: gc work buffer is misaligned");

	// Make sure all registers are saved on stack so that
	// scanstack sees them.
	__builtin_unwind_init();

	// The gc is turned off (via enablegc) until
	// the bootstrap has completed.
	// Also, malloc gets called in the guts
	// of a number of libraries that might be
	// holding locks.  To avoid priority inversion
	// problems, don't bother trying to run gc
	// while holding a lock.  The next mallocgc
	// without a lock will do the gc instead.
	m = runtime_m();
	if(!mstats.enablegc || m->locks > 0 || runtime_panicking)
		return;

	if(gcpercent == -2) {	// first time through
		p = runtime_getenv("GOGC");
		if(p == nil || p[0] == '\0')
			gcpercent = 100;
		else if(runtime_strcmp((const char*)p, "off") == 0)
			gcpercent = -1;
		else
			gcpercent = runtime_atoi(p);

		p = runtime_getenv("GOGCTRACE");
		if(p != nil)
			gctrace = runtime_atoi(p);
	}
	if(gcpercent < 0)
		return;

	runtime_semacquire(&runtime_worldsema);
	if(!force && mstats.heap_alloc < mstats.next_gc) {
		runtime_semrelease(&runtime_worldsema);
		return;
	}

	t0 = runtime_nanotime();

	m->gcing = 1;
	runtime_stoptheworld();

	for(m1=runtime_allm; m1; m1=m1->alllink)
		runtime_settype_flush(m1, false);

	heap0 = 0;
	obj0 = 0;
	if(gctrace) {
		cachestats(nil);
		heap0 = mstats.heap_alloc;
		obj0 = mstats.nmalloc - mstats.nfree;
	}

	work.nwait = 0;
	work.ndone = 0;
	work.debugmarkdone = 0;
	work.nproc = runtime_gcprocs();
	addroots();
	m->locks++;	// disable gc during mallocs in parforalloc
	if(work.markfor == nil)
		work.markfor = runtime_parforalloc(MaxGcproc);
	runtime_parforsetup(work.markfor, work.nproc, work.nroot, nil, false, markroot);
	if(work.sweepfor == nil)
		work.sweepfor = runtime_parforalloc(MaxGcproc);
	runtime_parforsetup(work.sweepfor, work.nproc, runtime_mheap.nspan, nil, true, sweepspan);
	m->locks--;
	if(work.nproc > 1) {
		runtime_noteclear(&work.alldone);
		runtime_helpgc(work.nproc);
	}

	runtime_parfordo(work.markfor);
	scanblock(nil, 0);

	if(DebugMark) {
		for(i=0; i<work.nroot; i++)
			debug_scanblock(work.roots[i].p, work.roots[i].n);
		runtime_atomicstore(&work.debugmarkdone, 1);
	}
	t1 = runtime_nanotime();

	runtime_parfordo(work.sweepfor);
	t2 = runtime_nanotime();

	stealcache();
	cachestats(&stats);

	if(work.nproc > 1)
		runtime_notesleep(&work.alldone);

	stats.nprocyield += work.sweepfor->nprocyield;
	stats.nosyield += work.sweepfor->nosyield;
	stats.nsleep += work.sweepfor->nsleep;

	mstats.next_gc = mstats.heap_alloc+(mstats.heap_alloc-runtime_stacks_sys)*gcpercent/100;
	m->gcing = 0;

	if(finq != nil) {
		m->locks++;	// disable gc during the mallocs in newproc
		// kick off or wake up goroutine to run queued finalizers
		if(fing == nil)
			fing = __go_go(runfinq, nil);
		else if(fingwait) {
			fingwait = 0;
			runtime_ready(fing);
		}
		m->locks--;
	}

	heap1 = mstats.heap_alloc;
	obj1 = mstats.nmalloc - mstats.nfree;

	t3 = runtime_nanotime();
	mstats.last_gc = t3;
	mstats.pause_ns[mstats.numgc%nelem(mstats.pause_ns)] = t3 - t0;
	mstats.pause_total_ns += t3 - t0;
	mstats.numgc++;
	if(mstats.debuggc)
		runtime_printf("pause %D\n", t3-t0);

	if(gctrace) {
		runtime_printf("gc%d(%d): %D+%D+%D ms, %D -> %D MB %D -> %D (%D-%D) objects,"
				" %D(%D) handoff, %D(%D) steal, %D/%D/%D yields\n",
			mstats.numgc, work.nproc, (t1-t0)/1000000, (t2-t1)/1000000, (t3-t2)/1000000,
			heap0>>20, heap1>>20, obj0, obj1,
			mstats.nmalloc, mstats.nfree,
			stats.nhandoff, stats.nhandoffcnt,
			work.sweepfor->nsteal, work.sweepfor->nstealcnt,
			stats.nprocyield, stats.nosyield, stats.nsleep);
	}

	runtime_MProf_GC();
	runtime_semrelease(&runtime_worldsema);
	runtime_starttheworld();

	// give the queued finalizers, if any, a chance to run
	if(finq != nil)
		runtime_gosched();

	if(gctrace > 1 && !force)
		runtime_gc(1);
}

void runtime_ReadMemStats(MStats *)
  __asm__("runtime.ReadMemStats");

void
runtime_ReadMemStats(MStats *stats)
{
	M *m;

	// Have to acquire worldsema to stop the world,
	// because stoptheworld can only be used by
	// one goroutine at a time, and there might be
	// a pending garbage collection already calling it.
	runtime_semacquire(&runtime_worldsema);
	m = runtime_m();
	m->gcing = 1;
	runtime_stoptheworld();
	cachestats(nil);
	*stats = mstats;
	m->gcing = 0;
	runtime_semrelease(&runtime_worldsema);
	runtime_starttheworld();
}

static void
runfinq(void* dummy __attribute__ ((unused)))
{
	Finalizer *f;
	FinBlock *fb, *next;
	uint32 i;

	for(;;) {
		// There's no need for a lock in this section
		// because it only conflicts with the garbage
		// collector, and the garbage collector only
		// runs when everyone else is stopped, and
		// runfinq only stops at the gosched() or
		// during the calls in the for loop.
		fb = finq;
		finq = nil;
		if(fb == nil) {
			fingwait = 1;
			runtime_park(nil, nil, "finalizer wait");
			continue;
		}
		if(raceenabled)
			runtime_racefingo();
		for(; fb; fb=next) {
			next = fb->next;
			for(i=0; i<(uint32)fb->cnt; i++) {
				void *params[1];

				f = &fb->fin[i];
				params[0] = &f->arg;
				reflect_call(f->ft, (void*)f->fn, 0, 0, params, nil);
				f->fn = nil;
				f->arg = nil;
			}
			fb->cnt = 0;
			fb->next = finc;
			finc = fb;
		}
		runtime_gc(1);	// trigger another gc to clean up the finalized objects, if possible
	}
}

// mark the block at v of size n as allocated.
// If noptr is true, mark it as having no pointers.
void
runtime_markallocated(void *v, uintptr n, bool noptr)
{
	uintptr *b, obits, bits, off, shift;

	if(0)
		runtime_printf("markallocated %p+%p\n", v, n);

	if((byte*)v+n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		runtime_throw("markallocated: bad pointer");

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;  // word offset
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	for(;;) {
		obits = *b;
		bits = (obits & ~(bitMask<<shift)) | (bitAllocated<<shift);
		if(noptr)
			bits |= bitNoPointers<<shift;
		if(runtime_singleproc) {
			*b = bits;
			break;
		} else {
			// more than one goroutine is potentially running: use atomic op
			if(runtime_casp((void**)b, (void*)obits, (void*)bits))
				break;
		}
	}
}

// mark the block at v of size n as freed.
void
runtime_markfreed(void *v, uintptr n)
{
	uintptr *b, obits, bits, off, shift;

	if(0)
		runtime_printf("markallocated %p+%p\n", v, n);

	if((byte*)v+n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		runtime_throw("markallocated: bad pointer");

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;  // word offset
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	for(;;) {
		obits = *b;
		bits = (obits & ~(bitMask<<shift)) | (bitBlockBoundary<<shift);
		if(runtime_singleproc) {
			*b = bits;
			break;
		} else {
			// more than one goroutine is potentially running: use atomic op
			if(runtime_casp((void**)b, (void*)obits, (void*)bits))
				break;
		}
	}
}

// check that the block at v of size n is marked freed.
void
runtime_checkfreed(void *v, uintptr n)
{
	uintptr *b, bits, off, shift;

	if(!runtime_checking)
		return;

	if((byte*)v+n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		return;	// not allocated, so okay

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;  // word offset
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	bits = *b>>shift;
	if((bits & bitAllocated) != 0) {
		runtime_printf("checkfreed %p+%p: off=%p have=%p\n",
			v, n, off, bits & bitMask);
		runtime_throw("checkfreed: not freed");
	}
}

// mark the span of memory at v as having n blocks of the given size.
// if leftover is true, there is left over space at the end of the span.
void
runtime_markspan(void *v, uintptr size, uintptr n, bool leftover)
{
	uintptr *b, off, shift;
	byte *p;

	if((byte*)v+size*n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		runtime_throw("markspan: bad pointer");

	p = v;
	if(leftover)	// mark a boundary just past end of last block too
		n++;
	for(; n-- > 0; p += size) {
		// Okay to use non-atomic ops here, because we control
		// the entire span, and each bitmap word has bits for only
		// one span, so no other goroutines are changing these
		// bitmap words.
		off = (uintptr*)p - (uintptr*)runtime_mheap.arena_start;  // word offset
		b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		*b = (*b & ~(bitMask<<shift)) | (bitBlockBoundary<<shift);
	}
}

// unmark the span of memory at v of length n bytes.
void
runtime_unmarkspan(void *v, uintptr n)
{
	uintptr *p, *b, off;

	if((byte*)v+n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		runtime_throw("markspan: bad pointer");

	p = v;
	off = p - (uintptr*)runtime_mheap.arena_start;  // word offset
	if(off % wordsPerBitmapWord != 0)
		runtime_throw("markspan: unaligned pointer");
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	n /= PtrSize;
	if(n%wordsPerBitmapWord != 0)
		runtime_throw("unmarkspan: unaligned length");
	// Okay to use non-atomic ops here, because we control
	// the entire span, and each bitmap word has bits for only
	// one span, so no other goroutines are changing these
	// bitmap words.
	n /= wordsPerBitmapWord;
	while(n-- > 0)
		*b-- = 0;
}

bool
runtime_blockspecial(void *v)
{
	uintptr *b, off, shift;

	if(DebugMark)
		return true;

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	return (*b & (bitSpecial<<shift)) != 0;
}

void
runtime_setblockspecial(void *v, bool s)
{
	uintptr *b, off, shift, bits, obits;

	if(DebugMark)
		return;

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	for(;;) {
		obits = *b;
		if(s)
			bits = obits | (bitSpecial<<shift);
		else
			bits = obits & ~(bitSpecial<<shift);
		if(runtime_singleproc) {
			*b = bits;
			break;
		} else {
			// more than one goroutine is potentially running: use atomic op
			if(runtime_casp((void**)b, (void*)obits, (void*)bits))
				break;
		}
	}
}

void
runtime_MHeap_MapBits(MHeap *h)
{
	size_t page_size;

	// Caller has added extra mappings to the arena.
	// Add extra mappings of bitmap words as needed.
	// We allocate extra bitmap pieces in chunks of bitmapChunk.
	enum {
		bitmapChunk = 8192
	};
	uintptr n;

	n = (h->arena_used - h->arena_start) / wordsPerBitmapWord;
	n = (n+bitmapChunk-1) & ~(bitmapChunk-1);
	if(h->bitmap_mapped >= n)
		return;

	page_size = getpagesize();
	n = (n+page_size-1) & ~(page_size-1);

	runtime_SysMap(h->arena_start - n, n - h->bitmap_mapped);
	h->bitmap_mapped = n;
}
