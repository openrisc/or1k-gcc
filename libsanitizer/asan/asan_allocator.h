//===-- asan_allocator.h ----------------------------------------*- C++ -*-===//
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is a part of AddressSanitizer, an address sanity checker.
//
// ASan-private header for asan_allocator.cc.
//===----------------------------------------------------------------------===//

#ifndef ASAN_ALLOCATOR_H
#define ASAN_ALLOCATOR_H

#include "asan_internal.h"
#include "asan_interceptors.h"

namespace __asan {

static const uptr kNumberOfSizeClasses = 255;
struct AsanChunk;

class AsanChunkView {
 public:
  explicit AsanChunkView(AsanChunk *chunk) : chunk_(chunk) {}
  bool IsValid() { return chunk_ != 0; }
  uptr Beg();       // first byte of user memory.
  uptr End();       // last byte of user memory.
  uptr UsedSize();  // size requested by the user.
  uptr AllocTid();
  uptr FreeTid();
  void GetAllocStack(StackTrace *stack);
  void GetFreeStack(StackTrace *stack);
  bool AddrIsInside(uptr addr, uptr access_size, uptr *offset);
  bool AddrIsAtLeft(uptr addr, uptr access_size, uptr *offset);
  bool AddrIsAtRight(uptr addr, uptr access_size, uptr *offset);
 private:
  AsanChunk *const chunk_;
};

AsanChunkView FindHeapChunkByAddress(uptr address);

class AsanChunkFifoList {
 public:
  explicit AsanChunkFifoList(LinkerInitialized) { }
  AsanChunkFifoList() { clear(); }
  void Push(AsanChunk *n);
  void PushList(AsanChunkFifoList *q);
  AsanChunk *Pop();
  uptr size() { return size_; }
  void clear() {
    first_ = last_ = 0;
    size_ = 0;
  }
 private:
  AsanChunk *first_;
  AsanChunk *last_;
  uptr size_;
};

struct AsanThreadLocalMallocStorage {
  explicit AsanThreadLocalMallocStorage(LinkerInitialized x)
      : quarantine_(x) { }
  AsanThreadLocalMallocStorage() {
    CHECK(REAL(memset));
    REAL(memset)(this, 0, sizeof(AsanThreadLocalMallocStorage));
  }

  AsanChunkFifoList quarantine_;
  AsanChunk *free_lists_[kNumberOfSizeClasses];
  void CommitBack();
};

// Fake stack frame contains local variables of one function.
// This struct should fit into a stack redzone (32 bytes).
struct FakeFrame {
  uptr magic;  // Modified by the instrumented code.
  uptr descr;  // Modified by the instrumented code.
  FakeFrame *next;
  u64 real_stack     : 48;
  u64 size_minus_one : 16;
};

struct FakeFrameFifo {
 public:
  void FifoPush(FakeFrame *node);
  FakeFrame *FifoPop();
 private:
  FakeFrame *first_, *last_;
};

class FakeFrameLifo {
 public:
  void LifoPush(FakeFrame *node) {
    node->next = top_;
    top_ = node;
  }
  void LifoPop() {
    CHECK(top_);
    top_ = top_->next;
  }
  FakeFrame *top() { return top_; }
 private:
  FakeFrame *top_;
};

// For each thread we create a fake stack and place stack objects on this fake
// stack instead of the real stack. The fake stack is not really a stack but
// a fast malloc-like allocator so that when a function exits the fake stack
// is not poped but remains there for quite some time until gets used again.
// So, we poison the objects on the fake stack when function returns.
// It helps us find use-after-return bugs.
// We can not rely on __asan_stack_free being called on every function exit,
// so we maintain a lifo list of all current fake frames and update it on every
// call to __asan_stack_malloc.
class FakeStack {
 public:
  FakeStack();
  explicit FakeStack(LinkerInitialized) {}
  void Init(uptr stack_size);
  void StopUsingFakeStack() { alive_ = false; }
  void Cleanup();
  uptr AllocateStack(uptr size, uptr real_stack);
  static void OnFree(uptr ptr, uptr size, uptr real_stack);
  // Return the bottom of the maped region.
  uptr AddrIsInFakeStack(uptr addr);
  bool StackSize() { return stack_size_; }

 private:
  static const uptr kMinStackFrameSizeLog = 9;  // Min frame is 512B.
  static const uptr kMaxStackFrameSizeLog = 16;  // Max stack frame is 64K.
  static const uptr kMaxStackMallocSize = 1 << kMaxStackFrameSizeLog;
  static const uptr kNumberOfSizeClasses =
      kMaxStackFrameSizeLog - kMinStackFrameSizeLog + 1;

  bool AddrIsInSizeClass(uptr addr, uptr size_class);

  // Each size class should be large enough to hold all frames.
  uptr ClassMmapSize(uptr size_class);

  uptr ClassSize(uptr size_class) {
    return 1UL << (size_class + kMinStackFrameSizeLog);
  }

  void DeallocateFrame(FakeFrame *fake_frame);

  uptr ComputeSizeClass(uptr alloc_size);
  void AllocateOneSizeClass(uptr size_class);

  uptr stack_size_;
  bool   alive_;

  uptr allocated_size_classes_[kNumberOfSizeClasses];
  FakeFrameFifo size_classes_[kNumberOfSizeClasses];
  FakeFrameLifo call_stack_;
};

void *asan_memalign(uptr alignment, uptr size, StackTrace *stack);
void asan_free(void *ptr, StackTrace *stack);

void *asan_malloc(uptr size, StackTrace *stack);
void *asan_calloc(uptr nmemb, uptr size, StackTrace *stack);
void *asan_realloc(void *p, uptr size, StackTrace *stack);
void *asan_valloc(uptr size, StackTrace *stack);
void *asan_pvalloc(uptr size, StackTrace *stack);

int asan_posix_memalign(void **memptr, uptr alignment, uptr size,
                          StackTrace *stack);
uptr asan_malloc_usable_size(void *ptr, StackTrace *stack);

uptr asan_mz_size(const void *ptr);
void asan_mz_force_lock();
void asan_mz_force_unlock();

}  // namespace __asan
#endif  // ASAN_ALLOCATOR_H
