/* Macros for arch_atomic functionality for or1k.
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


/* Provides macros for common arch_atomic functionality.  */

#ifndef _ATOMIC_H_
#define _ATOMIC_H_

/* Or1k has no arch_atomic compare-and-exchange operation, but the
   kernel provides userspace arch_atomicity operations.  Use them.  */

/* TODO(bluecmd): Ugly, but needed for now */
#define __NR_or1k_atomic 244

/* Copy/paste from glibc. */
#undef INTERNAL_SYSCALL
#define INTERNAL_SYSCALL(nr, args...) \
	({ unsigned long __sys_result;						\
	  {									\
	    register long __sc_ret __asm__ ("r11") = __NR_or1k_atomic;		\
	    LOAD_ARGS_##nr (args)						\
	    __asm__ __volatile__ ("l.sys     1"					\
					   : "=r" (__sc_ret) ASM_ARGS_OUT_##nr	\
					   : "0" (__sc_ret) ASM_ARGS_IN_##nr	\
			 : ASM_CLOBBERS_##nr					\
			   "r12", "r13", "r15", "r17", "r19",			\
			   "r21", "r23", "r25", "r27", "r29",			\
			   "r31", "memory");					\
	    __asm__ __volatile__ ("l.nop");					\
	    __sys_result = __sc_ret;						\
	  }									\
	  (long) __sys_result; })


/* TODO: Move these to a kernel header */
#define OR1K_ATOMIC_SWAP	1
#define OR1K_ATOMIC_CMPXCHG	2
#define OR1K_ATOMIC_XCHG	3
#define OR1K_ATOMIC_ADD		4
#define OR1K_ATOMIC_DECPOS	5
#define OR1K_ATOMIC_AND		6
#define OR1K_ATOMIC_OR		7
#define OR1K_ATOMIC_UMAX	8
#define OR1K_ATOMIC_UMIN	9

#define arch_atomic_val_compare_and_exchange(mem, newval, oldval) \
  ((__typeof (*(mem))) ((sizeof (*(mem)) == 4) ? \
	 INTERNAL_SYSCALL (4, OR1K_ATOMIC_CMPXCHG, mem, oldval, newval) \
	 : __atomic_error_bad_argument_size ()))

#define arch_atomic_exchange(mem, newval) \
  ((__typeof (*(mem))) ((sizeof (*(mem)) == 4) ? \
	 INTERNAL_SYSCALL (3, OR1K_ATOMIC_XCHG, mem, newval) \
	 : __atomic_error_bad_argument_size ()))

#define arch_atomic_exchange_and_add(mem, val) \
  ((__typeof (*(mem))) ((sizeof (*(mem)) == 4) ? \
	 INTERNAL_SYSCALL (3, OR1K_ATOMIC_ADD, mem, val) \
	 : __atomic_error_bad_argument_size ()))

#define arch_atomic_decrement_if_positive(mem) \
  ((__typeof (*(mem))) ((sizeof (*(mem)) == 4) ? \
	 INTERNAL_SYSCALL (2, OR1K_ATOMIC_DECPOS, mem) \
	 : __atomic_error_bad_argument_size ()))

#define arch_atomic_and_val(mem, mask) \
  ((__typeof (*(mem))) ((sizeof (*(mem)) == 4) ? \
	 INTERNAL_SYSCALL (3, OR1K_ATOMIC_AND, mem, mask) \
	 : __atomic_error_bad_argument_size ()))

#define arch_atomic_or_val(mem, mask) \
  ((__typeof (*(mem))) ((sizeof (*(mem)) == 4) ? \
	 INTERNAL_SYSCALL (3, OR1K_ATOMIC_OR, mem, mask) \
	 : __atomic_error_bad_argument_size ()))

#define arch_atomic_max_val(mem, val) \
  ((__typeof (*(mem))) ((sizeof (*(mem)) == 4) ? \
	 INTERNAL_SYSCALL (3, OR1K_ATOMIC_UMAX, mem, val) \
	 : __atomic_error_bad_argument_size ()))

#define arch_atomic_min_val(mem, val) \
  ((__typeof (*(mem))) ((sizeof (*(mem)) == 4) ? \
	 INTERNAL_SYSCALL (3, OR1K_ATOMIC_UMIN, mem, val) \
	 : __atomic_error_bad_argument_size ()))

/*
 * This non-existent symbol is called for unsupporrted sizes,
 * indicating a bug in the caller.
 */
extern int __atomic_error_bad_argument_size(void)
    __attribute__ ((error ("bad sizeof atomic argument")));

/* Copied from tilepro. TODO(bluecmd): Barriers might be needed when it's not
   a huge syscall. */
#define arch_atomic_compiler_barrier() __asm__ __volatile__("" ::: "memory")
#define arch_atomic_full_barrier() __asm__ __volatile__("l.nop" ::: "memory")
#define arch_atomic_write_barrier() arch_atomic_full_barrier()
#define arch_atomic_read_barrier() arch_atomic_full_barrier()
#define arch_atomic_acquire_barrier() arch_atomic_compiler_barrier()

/* Copy/paste from glibc. */
#define LOAD_ARGS_0()

#define ASM_ARGS_OUT_0
#define ASM_ARGS_IN_0
#define ASM_CLOBBERS_0  "r3", ASM_CLOBBERS_1

#define LOAD_ARGS_1(a)        \
    LOAD_ARGS_0 ()        \
  register long __a __asm__ ("r3") = (long)(a);
#define ASM_ARGS_OUT_1  ASM_ARGS_OUT_0, "=r" (__a)
#define ASM_ARGS_IN_1 ASM_ARGS_IN_0, "1" (__a)
#define ASM_CLOBBERS_1  "r4", ASM_CLOBBERS_2

#define LOAD_ARGS_2(a, b)     \
    LOAD_ARGS_1 (a)       \
  register long __b __asm__ ("r4") = (long)(b);
#define ASM_ARGS_OUT_2  ASM_ARGS_OUT_1, "=r" (__b)
#define ASM_ARGS_IN_2 ASM_ARGS_IN_1, "2" (__b)
#define ASM_CLOBBERS_2  "r5", ASM_CLOBBERS_3

#define LOAD_ARGS_3(a, b, c)      \
    LOAD_ARGS_2 (a, b)        \
  register long __c __asm__ ("r5") = (long)(c);
#define ASM_ARGS_OUT_3  ASM_ARGS_OUT_2, "=r" (__c)
#define ASM_ARGS_IN_3 ASM_ARGS_IN_2, "3" (__c)
#define ASM_CLOBBERS_3  "r6", ASM_CLOBBERS_4

#define LOAD_ARGS_4(a, b, c, d)     \
    LOAD_ARGS_3 (a, b, c)       \
  register long __d __asm__ ("r6") = (long)(d);
#define ASM_ARGS_OUT_4  ASM_ARGS_OUT_3, "=r" (__d)
#define ASM_ARGS_IN_4 ASM_ARGS_IN_3, "4" (__d)
#define ASM_CLOBBERS_4  "r7", ASM_CLOBBERS_5

#define LOAD_ARGS_5(a, b, c, d, e)    \
    LOAD_ARGS_4 (a, b, c, d)      \
  register long __e __asm__ ("r7") = (long)(e);
#define ASM_ARGS_OUT_5  ASM_ARGS_OUT_4, "=r" (__e)
#define ASM_ARGS_IN_5 ASM_ARGS_IN_4, "5" (__e)
#define ASM_CLOBBERS_5  "r8", ASM_CLOBBERS_6

#define LOAD_ARGS_6(a, b, c, d, e, f)   \
    LOAD_ARGS_5 (a, b, c, d, e)     \
  register long __f __asm__ ("r8") = (long)(f);
#define ASM_ARGS_OUT_6  ASM_ARGS_OUT_5, "=r" (__f)
#define ASM_ARGS_IN_6 ASM_ARGS_IN_5, "6" (__f)
#define ASM_CLOBBERS_6

#endif /* !_ATOMIC_H_ */
