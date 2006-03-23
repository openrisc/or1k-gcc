/* Copyright (C) 2004 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Calculate division table for SH2..4 integer division
   Contributed by Joern Rernnecke
   joern.rennecke@superh.com  */

#include <stdio.h>
#include <math.h>

int
main ()
{
  int i, j;
  double q, r, err, max_err = 0, max_s_err = 0;

  puts("/* This table has been generated by divtab-sh4.c.  */");
  puts ("\t.balign 4");
  puts ("LOCAL(div_table_clz):");
  /* output some dummy number for 1/0.  */
  printf ("\t.byte\t%d\n", 0);
  for (i = 1; i <= 128; i++)
    {
      int n = 0;
      if (i == 128)
	puts ("\
/* Lookup table translating positive divisor to index into table of\n\
   normalized inverse.  N.B. the '0' entry is also the last entry of the\n\
 previous table, and causes an unaligned access for division by zero.  */\n\
LOCAL(div_table_ix):");
      for (j = i; j <= 128; j += j)
	n++;
      printf ("\t.byte\t%d\n", n - 7);
    }
  for (i = 1; i <= 128; i++)
    {
      j = i < 0 ? -i : i;
      while (j < 128)
	j += j;
      printf ("\t.byte\t%d\n", j * 2 - 96*4);
    }
  puts("\
/* 1/64 .. 1/127, normalized.  There is an implicit leading 1 in bit 32.  */\n\
	.balign 4\n\
LOCAL(zero_l):");
  for (i = 64; i < 128; i++)
    {
      if (i == 96)
	puts ("LOCAL(div_table):");
      q = 4.*(1<<30)*128/i;
      r = ceil (q);
      /* The value for 64 is actually differently scaled that it would
	 appear from this calculation.  The implicit part is %01, not 10.
	 Still, since the value in the table is 0 either way, this
	 doesn't matter here.  Still, the 1/64 entry is effectively a 1/128
	 entry.  */
      printf ("\t.long\t0x%X\n", (unsigned) r);
      err = r - q;
      if (err > max_err)
	max_err = err;
      err = err * i / 128;
      if (err > max_s_err)
	max_s_err = err;
    }
  printf ("\t/* maximum error: %f scaled: %f*/\n", max_err, max_s_err);
  exit (0);
}
