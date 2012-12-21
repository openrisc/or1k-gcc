//===-- asan_stack.cc -----------------------------------------------------===//
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is a part of AddressSanitizer, an address sanity checker.
//
// Code for ASan stack trace.
//===----------------------------------------------------------------------===//
#include "asan_flags.h"
#include "asan_stack.h"
#include "sanitizer/asan_interface.h"

namespace __asan {

void PrintStack(StackTrace *stack) {
  stack->PrintStack(stack->trace, stack->size, flags()->symbolize,
                    flags()->strip_path_prefix, __asan_symbolize);
}

}  // namespace __asan

// ------------------ Interface -------------- {{{1

// Provide default implementation of __asan_symbolize that does nothing
// and may be overriden by user if he wants to use his own symbolization.
// ASan on Windows has its own implementation of this.
#ifndef _WIN32
SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE NOINLINE
bool __asan_symbolize(const void *pc, char *out_buffer, int out_size) {
  return false;
}
#endif
