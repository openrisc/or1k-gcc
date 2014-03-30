/* Or1k atomics.
   Copyright (C) 2014 Free Software Foundation, Inc.
   Contributed by Christian Svensson (blue@cmd.nu)

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "tconfig.h"
#include "coretypes.h"
#include "atomic.h"

#define bool unsigned char
#define __unused __attribute__((unused))

/* TODO(bluecmd): I have only implemented same basic ones here that I need,
   the real solution would be to implement atomic instructions in or1k. */

bool __sync_bool_compare_and_swap_4(int* ptr, int oldval, int newval) {
  arch_atomic_write_barrier();
  int retval = arch_atomic_val_compare_and_exchange(ptr, oldval, newval);
  arch_atomic_read_barrier();
  return retval;
}

int __sync_val_compare_and_swap_4(int *ptr, int oldval, int newval) {
  arch_atomic_write_barrier();
  int retval = arch_atomic_val_compare_and_exchange(ptr, oldval, newval);
  arch_atomic_read_barrier();
  return retval;
}

bool __atomic_compare_exchange_4(volatile int* ptr, int* oldvalp,
				 int newval, bool weak __unused,
				 int models, int modelf __unused) {
  int oldval = *oldvalp;
  __atomic_thread_fence (models);
  int retval = arch_atomic_val_compare_and_exchange(ptr, oldval, newval);
  __atomic_thread_fence (models);
  bool success = (retval == oldval);
  *oldvalp = retval;
  return success;
}

int __atomic_exchange_4(volatile int *ptr, int val, int model) {
  __atomic_thread_fence (model);
  int retval = arch_atomic_exchange(ptr, val);
  __atomic_thread_fence (model);
  return retval;
}

int __atomic_fetch_add_4(volatile int *ptr, int val, int model) {
  __atomic_thread_fence (model);
  int retval = arch_atomic_exchange_and_add(ptr, val);
  __atomic_thread_fence (model);
  return retval;
}
