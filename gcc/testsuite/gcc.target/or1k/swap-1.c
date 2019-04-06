/* { dg-do run } */
/* { dg-options "-Os -mhard-mul -msoft-div -msoft-float" } */

/* Copyright (C) 2018-2019 Free Software Foundation, Inc.
   Copyright 2019 Broadcom.   Richard Selvaggi, 2019-March-27
   The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License, version 2, as
   published by the Free Software Foundation (the "GPL").

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License version 2 (GPLv2) for more details.

   You should have received a copy of the GNU General Public License
   version 2 (GPLv2) along with this source code.  */

/* Notes:

   This test failed on or1k GCC 7.2.0, and passes on or1k GCC 5.3.0
   as well as the or1k port released in GCC 9.1.

   The main program is organized as a loop structure so gcc does not
   optimize-away the calls to swap_1().  Compiling with -O2 is still smart
   enough to optimize-away the calls, but using -Os does not.
   The bad code is only generated when compiled with -Os.

   When the bad code is generated all code is okay except for the very last
   instruction (a 'l.addc' in the l.jr delay slot).
   Up to that point in execution, r11 and r12 contain the correct (expected)
   values, but the execution of the final "l.addc" corrupts r11.

   This test is added to ensure this does not come back.  */

#include <stdint.h>

volatile static uint8_t g_doswap = 1;

uint64_t swap_1 (uint64_t u64) {
  uint32_t u64_lo, u64_hi, u64_tmp;

  u64_lo = u64 & 0xFFFFFFFF;
  u64_hi = u64 >> 32;

  if (g_doswap)
    {
      u64_tmp = u64_lo;
      u64_lo  = u64_hi;
      u64_hi  = u64_tmp;
    }

  u64 = u64_lo;
  u64 += ((uint64_t) u64_hi << 32);

  return u64;
}

int main () {
  int ret;
  int iter;
  uint64_t  aa[2];   // inputs to swap function
  uint64_t  ee[2];   // expected outputs of swap function
  uint64_t  rr[2];   // actual results of swap function

  g_doswap = 1;

  // populate inputs, and expected outputs:
  aa[0] = 0x123456789abcdef0;
  aa[1] = 0x0123456789abcdef;

  ee[0] = 0x9ABCDEF012345678;
  ee[1] = 0x89ABCDEF01234567;

  ret = 0;
  for (iter = 0; iter < 2; iter++)
    {
      rr[iter] = swap_1(aa[iter]);
      // early-out if there's a mis-match:
      if (ee[iter] != rr[iter])
        ret = 1;
    }

  return ret;
}
