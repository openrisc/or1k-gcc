/* { dg-do run } */
/* { dg-require-effective-target arm_eabi } */
/* { dg-require-effective-target arm_arch_v6t2_multilib } */
/* { dg-options "-mthumb" } */
/* { dg-add-options arm_arch_v6t2 } */

#include "ftest-support-thumb.h"

int
main (void)
{
  return ftest (ARCH_V6T2);
}

