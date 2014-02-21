/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -mabi=altivec -std=gnu99 -mvsx" } */

#include "harness.h"

static unsigned long svul[2] __attribute__ ((aligned (16)));
static double svd[2] __attribute__ ((aligned (16)));

static void check_arrays ()
{
  unsigned int i;
  for (i = 0; i < 2; ++i)
    {
      check (svul[i] == i, "svul");
      check (svd[i] == i * 1.0, "svd");
    }
}

static void test ()
{
  vector unsigned long vul = {0,1};
  vector double vd = {0.0,1.0};

  vec_stl (vul, 0, (vector unsigned long *)svul);
  vec_stl (vd,  0, (vector double *)svd);

  check_arrays ();
}
