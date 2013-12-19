/* ARM Non-NEON ACLE intrinsics include file.

   Copyright (C) 2013 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _GCC_ARM_ACLE_H
#define _GCC_ARM_ACLE_H

#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

#ifdef __ARM_FEATURE_CRC32
__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32b (uint32_t a, uint8_t b)
{
  return __builtin_arm_crc32b (a, b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32h (uint32_t a, uint16_t b)
{
  return __builtin_arm_crc32h (a, b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32w (uint32_t a, uint32_t b)
{
  return __builtin_arm_crc32w (a, b);
}

#ifdef __ARM_32BIT_STATE
__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32d (uint32_t a, uint64_t b)
{
  uint32_t d;

  d = __crc32w (__crc32w (a, b & 0xffffffffULL), b >> 32);
  return d;
}
#endif

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32cb (uint32_t a, uint8_t b)
{
  return __builtin_arm_crc32cb (a, b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32ch (uint32_t a, uint16_t b)
{
  return __builtin_arm_crc32ch (a, b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32cw (uint32_t a, uint32_t b)
{
  return __builtin_arm_crc32cw (a, b);
}

#ifdef __ARM_32BIT_STATE
__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32cd (uint32_t a, uint64_t b)
{
  uint32_t d;

  d = __crc32cw (__crc32cw (a, b & 0xffffffffULL), b >> 32);
  return d;
}
#endif

#endif

#ifdef __cplusplus
}
#endif

#endif
