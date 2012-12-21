/* Machine description for AArch64 architecture.
   Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define _FP_W_TYPE_SIZE		64
#define _FP_W_TYPE		unsigned long
#define _FP_WS_TYPE		signed long
#define _FP_I_TYPE		int

typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));
#define TI_BITS (__CHAR_BIT__ * (int)sizeof(TItype))

/* The type of the result of a floating point comparison.  This must
   match __libgcc_cmp_return__ in GCC for the target.  */
typedef int __gcc_CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));
#define CMPtype __gcc_CMPtype

#define _FP_MUL_MEAT_Q(R,X,Y)				\
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_Q(R,X,Y)	_FP_DIV_MEAT_2_udiv(Q,R,X,Y)

#define _FP_NANFRAC_S		((_FP_QNANBIT_S << 1) - 1)
#define _FP_NANFRAC_D		((_FP_QNANBIT_D << 1) - 1)
#define _FP_NANFRAC_Q		((_FP_QNANBIT_Q << 1) - 1), -1
#define _FP_NANSIGN_S		0
#define _FP_NANSIGN_D		0
#define _FP_NANSIGN_Q		0

#define _FP_KEEPNANFRACP 1

/* This appears to be in line with the VFP conventions in the v7-a
   ARM-ARM. Need to check with the v8 version.  */
#define _FP_CHOOSENAN(fs, wc, R, X, Y, OP)			\
  do {								\
    if ((_FP_FRAC_HIGH_RAW_##fs(X) & _FP_QNANBIT_##fs)		\
	&& !(_FP_FRAC_HIGH_RAW_##fs(Y) & _FP_QNANBIT_##fs))	\
      {								\
	R##_s = Y##_s;						\
	_FP_FRAC_COPY_##wc(R,Y);				\
      }								\
    else							\
      {								\
	R##_s = X##_s;						\
	_FP_FRAC_COPY_##wc(R,X);				\
      }								\
    R##_c = FP_CLS_NAN;						\
  } while (0)

#define FP_EX_INVALID	0x01
#define FP_EX_DIVZERO	0x02
#define FP_EX_OVERFLOW	0x04
#define FP_EX_UNDERFLOW	0x08
#define FP_EX_INEXACT	0x10

#define FP_HANDLE_EXCEPTIONS						\
  do {									\
    const float fp_max = __FLT_MAX__;					\
    const float fp_min = __FLT_MIN__;					\
    const float fp_1e32 = 1.0e32f;					\
    const float fp_zero = 0.0;						\
    const float fp_one = 1.0;						\
    unsigned fpsr;							\
    if (_fex & FP_EX_INVALID)						\
      {									\
        __asm__ __volatile__ ("fdiv\ts0, %s0, %s0"			\
			      :						\
			      : "w" (fp_zero)				\
			      : "s0");					\
	__asm__ __volatile__ ("mrs\t%0, fpsr" : "=r" (fpsr));		\
      }									\
    if (_fex & FP_EX_DIVZERO)						\
      {									\
	__asm__ __volatile__ ("fdiv\ts0, %s0, %s1"			\
			      :						\
			      : "w" (fp_one), "w" (fp_zero)		\
			      : "s0");					\
	__asm__ __volatile__ ("mrs\t%0, fpsr" : "=r" (fpsr));		\
      }									\
    if (_fex & FP_EX_OVERFLOW)						\
      {									\
        __asm__ __volatile__ ("fadd\ts0, %s0, %s1"			\
			      :						\
			      : "w" (fp_max), "w" (fp_1e32)		\
			      : "s0");					\
        __asm__ __volatile__ ("mrs\t%0, fpsr" : "=r" (fpsr));		\
      }									\
    if (_fex & FP_EX_UNDERFLOW)						\
      {									\
	__asm__ __volatile__ ("fmul\ts0, %s0, %s0"			\
			      :						\
			      : "w" (fp_min)				\
			      : "s0");					\
	__asm__ __volatile__ ("mrs\t%0, fpsr" : "=r" (fpsr));		\
      }									\
    if (_fex & FP_EX_INEXACT)						\
      {									\
	__asm__ __volatile__ ("fsub\ts0, %s0, %s1"			\
			      :						\
			      : "w" (fp_max), "w" (fp_one)		\
			      : "s0");					\
	__asm__ __volatile__ ("mrs\t%0, fpsr" : "=r" (fpsr));		\
      }									\
  } while (0)


#define FP_RND_NEAREST		0
#define FP_RND_ZERO		0xc00000
#define FP_RND_PINF		0x400000
#define FP_RND_MINF		0x800000

#define _FP_DECL_EX \
  unsigned long int _fpcr __attribute__ ((unused)) = FP_RND_NEAREST

#define FP_INIT_ROUNDMODE			\
  do {						\
    __asm__ __volatile__ ("mrs	%0, fpcr"	\
			  : "=r" (_fpcr));	\
  } while (0)

#define FP_ROUNDMODE (_fpcr & 0xc00000)

#define	__LITTLE_ENDIAN	1234
#define	__BIG_ENDIAN	4321

#if defined __AARCH64EB__
# define __BYTE_ORDER __BIG_ENDIAN
#else
# define __BYTE_ORDER __LITTLE_ENDIAN
#endif


/* Define ALIASNAME as a strong alias for NAME.  */
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));
