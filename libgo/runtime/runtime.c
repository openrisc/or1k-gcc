// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <unistd.h>

#include "config.h"

#include "runtime.h"
#include "array.h"
#include "go-panic.h"

int32
runtime_gotraceback(void)
{
	const byte *p;

	p = runtime_getenv("GOTRACEBACK");
	if(p == nil || p[0] == '\0')
		return 1;	// default is on
	return runtime_atoi(p);
}

static int32	argc;
static byte**	argv;

extern Slice os_Args asm ("os.Args");
extern Slice syscall_Envs asm ("syscall.Envs");

void (*runtime_sysargs)(int32, uint8**);

void
runtime_args(int32 c, byte **v)
{
	argc = c;
	argv = v;
	if(runtime_sysargs != nil)
		runtime_sysargs(c, v);
}

byte*
runtime_progname()
{
  return argc == 0 ? nil : argv[0];
}

void
runtime_goargs(void)
{
	String *s;
	int32 i;

	// for windows implementation see "os" package
	if(Windows)
		return;

	s = runtime_malloc(argc*sizeof s[0]);
	for(i=0; i<argc; i++)
		s[i] = runtime_gostringnocopy((const byte*)argv[i]);
	os_Args.__values = (void*)s;
	os_Args.__count = argc;
	os_Args.__capacity = argc;
}

void
runtime_goenvs_unix(void)
{
	String *s;
	int32 i, n;

	for(n=0; argv[argc+1+n] != 0; n++)
		;

	s = runtime_malloc(n*sizeof s[0]);
	for(i=0; i<n; i++)
		s[i] = runtime_gostringnocopy(argv[argc+1+i]);
	syscall_Envs.__values = (void*)s;
	syscall_Envs.__count = n;
	syscall_Envs.__capacity = n;
}

const byte*
runtime_getenv(const char *s)
{
	int32 i, j, len;
	const byte *v, *bs;
	String* envv;
	int32 envc;

	bs = (const byte*)s;
	len = runtime_findnull(bs);
	envv = (String*)syscall_Envs.__values;
	envc = syscall_Envs.__count;
	for(i=0; i<envc; i++){
		if(envv[i].len <= len)
			continue;
		v = (const byte*)envv[i].str;
		for(j=0; j<len; j++)
			if(bs[j] != v[j])
				goto nomatch;
		if(v[len] != '=')
			goto nomatch;
		return v+len+1;
	nomatch:;
	}
	return nil;
}

int32
runtime_atoi(const byte *p)
{
	int32 n;

	n = 0;
	while('0' <= *p && *p <= '9')
		n = n*10 + *p++ - '0';
	return n;
}

uint32
runtime_fastrand1(void)
{
	M *m;
	uint32 x;

	m = runtime_m();
	x = m->fastrand;
	x += x;
	if(x & 0x80000000L)
		x ^= 0x88888eefUL;
	m->fastrand = x;
	return x;
}

static struct root_list runtime_roots =
{ nil,
  { { &syscall_Envs, sizeof syscall_Envs },
    { &os_Args, sizeof os_Args },
    { nil, 0 } },
};

void
runtime_check(void)
{
	__go_register_gc_roots(&runtime_roots);
}

int64
runtime_cputicks(void)
{
#if defined(__386__) || defined(__x86_64__)
  uint32 low, high;
  asm("rdtsc" : "=a" (low), "=d" (high));
  return (int64)(((uint64)high << 32) | (uint64)low);
#else
  // FIXME: implement for other processors.
  return 0;
#endif
}

bool
runtime_showframe(String s)
{
	static int32 traceback = -1;
	
	if(traceback < 0)
		traceback = runtime_gotraceback();
	return traceback > 1 || (__builtin_memchr(s.str, '.', s.len) != nil && __builtin_memcmp(s.str, "runtime.", 7) != 0);
}

static Lock ticksLock;
static int64 ticks;

int64
runtime_tickspersecond(void)
{
	int64 res, t0, t1, c0, c1;

	res = (int64)runtime_atomicload64((uint64*)&ticks);
	if(res != 0)
		return ticks;
	runtime_lock(&ticksLock);
	res = ticks;
	if(res == 0) {
		t0 = runtime_nanotime();
		c0 = runtime_cputicks();
		runtime_usleep(100*1000);
		t1 = runtime_nanotime();
		c1 = runtime_cputicks();
		if(t1 == t0)
			t1++;
		res = (c1-c0)*1000*1000*1000/(t1-t0);
		if(res == 0)
			res++;
		runtime_atomicstore64((uint64*)&ticks, res);
	}
	runtime_unlock(&ticksLock);
	return res;
}

int64 runtime_pprof_runtime_cyclesPerSecond(void)
     asm("runtime_pprof.runtime_cyclesPerSecond");

int64
runtime_pprof_runtime_cyclesPerSecond(void)
{
	return runtime_tickspersecond();
}
