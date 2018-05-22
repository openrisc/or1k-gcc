/* Copyright (C) 2018 Free Software Foundation, Inc.

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


#ifdef L__mulsi3
	.balign 4
	.globl	__mulsi3
	.type	__mulsi3, @function
__mulsi3:
	l.movhi		r11, 0			/* initial r */

	/* Given R = X * Y ... */
1:	l.sfeqi		r4, 0			/* while (y != 0) */
	l.bf		2f
	 l.andi		r5, r4, 1		/* if (y & 1) ... */
	l.add		r12, r11, r3
	l.sfnei		r5, 0
	l.cmov		r11, r12, r11		/* ... r += x. */
	l.srli		r4, r4, 1		/* y >>= 1 */
	l.j		1b
	 l.add		r3, r3, r3		/* x <<= 1 */

2:	l.jr		r9
	 l.nop

	.size	__mulsi3, . - __mulsi3
#endif

#if defined(L__udivsi3) || defined(L__umodsi3) \
    || defined(L__divsi3) || defined(L__modsi3)
	.global	__udivmodsi3_internal
	.hidden	__udivmodsi3_internal
	.type	__udivmodsi3_internal, @function
#endif

#ifdef L__udivsi3
	.balign	4
	.global	__udivsi3
	.type	__udivsi3, @function
__udivsi3:
__udivmodsi3_internal:
	/* Note that the other division routines assume that r13
	   is not clobbered by this routine, and use that as to
	   save a return address without creating a stack frame.  */

        l.sfeqi		r4, 0		/* division by zero; return 0.  */
        l.ori		r11, r0, 0	/* initial quotient */
	l.bf		9f
	 l.ori		r12, r3, 0	/* initial remainder */

	/* Given X/Y, shift Y left until Y >= X.  */
	l.ori		r6, r0, 1	/* mask = 1 */
1:	l.sfltsi	r4, 0		/* y has msb set */
	l.bf		2f
	 l.sfltu	r4, r12		/* y < x */
	l.add		r4, r4, r4	/* y <<= 1 */
	l.bnf		1b
	 l.add		r6, r6, r6	/* mask <<= 1 */

	/* Shift Y back to the right again, subtracting from X.  */
2:	l.add		r7, r11, r6	/* tmp1 = quot + mask */
3:	l.srli		r6, r6, 1	/* mask >>= 1 */
	l.sub		r8, r12, r4	/* tmp2 = x - y */
	l.sfleu		r4, r12		/* y <= x */
	l.srli		r4, r4, 1	/* y >>= 1 */
	l.cmov		r11, r7, r11	/* if (y <= x) quot = tmp1 */
	l.cmov		r12, r8, r12	/* if (y <= x) x = tmp2 */
	l.sfnei		r6, 0		/* loop until mask == 0 */
	l.bf		3b
	 l.add		r7, r11, r6	/* delay fill from loop start */

9:	l.jr		r9
	 l.nop

	.size	__udivsi3, . - __udivsi3
	.size	__udivmodsi3_internal, . - __udivmodsi3_internal
#endif

#ifdef L__umodsi3
	.balign	4
	.global	__umodsi3
	.type	__umodsi3, @function
	.cfi_startproc
__umodsi3:
	/* Know that __udivmodsi3_internal does not clobber r13.  */
	l.ori		r13, r9, 0
	.cfi_register	9, 13
	l.jal		__udivmodsi3_internal
	 l.nop
	l.jr		r13		/* return to saved lr */
	 l.ori		r11, r12, 0	/* move remainder to rv */

	.cfi_endproc
	.size	__umodsi3, . - __umodsi3
#endif

/* For signed division we do:
 *
 *   -x / y = x / -y = -(x / y)
 *   -x % y = -(x % y)
 *   x % -y = x % b
 *
 * which has the property that (x/y)*y + (x%y) = x.
 */

#ifdef L__divsi3
	.balign	4
	.global	__divsi3
	.type	__divsi3, @function
	.cfi_startproc
__divsi3:
	l.xor		r6, r3, r4	/* need result negate?  */

	l.sfltsi	r3, 0		/* abs(x) */
	l.sub		r5, r0, r3
	l.cmov		r3, r5, r3

	l.sfltsi	r4, 0		/* abs(y) */
	l.sub		r5, r0, r4
	l.cmov		r4, r5, r4

	/* If the result will not require sign flip, tail call.  */
	l.sfltsi	r6, 0
        l.bnf		__udivmodsi3_internal
	 l.ori		r13, r9, 0	/* save lr */

	/* Otherwise, know that __udivmodsi3_internal does not clobber r13.
	   Perform a normal call, then negate and return via saved lr.  */
	.cfi_register	9, 13
        l.jal		__udivmodsi3_internal
	 l.nop
	l.jr		r13
	 l.sub		r11, r0, r11

	.cfi_endproc
	.size	__divsi3, . - __divsi3
#endif

#ifdef L__modsi3
	.balign	4
	.global	__modsi3
	.type	__modsi3, @function
	.cfi_startproc
__modsi3:
	l.sfltsi	r4, 0		/* abs(y) */
	l.sub		r5, r0, r4
	l.cmov		r4, r5, r4

	l.sfltsi	r3, 0		/* x negative? */
	l.bf		1f
	 l.ori		r13, r9, 0	/* save lr */

	/* Know that __udivmodsi3_internal does not clobber r13.  */
	.cfi_register	9, 13

	/* X positive; no negate of the result required.  */
	l.jal		__udivmodsi3_internal
	 l.nop
	l.jr		r13		/* return to saved lr */
	 l.ori		r11, r12, 0	/* move remainder to rv */

	/* X negative; negate both X and the result.  */
1:	l.jal		__udivmodsi3_internal
	 l.sub		r3, r0, r3
	l.jr		r13		/* return to saved lr */
	 l.sub		r11, r0, r12	/* negate remainder to rv */

	.cfi_endproc
	.size __modsi3, .- __modsi3
#endif
