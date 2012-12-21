/* { dg-do run } */
/* { dg-require-effective-target arm_nothumb } */
/* { dg-require-effective-target arm_arch_v7a_multilib } */
/* { dg-options "-marm" } */
/* { dg-add-options arm_arch_v7a } */

#include "ftest-support-arm.h"

int
main (void)
{
  return ftest (ARCH_V7A);
}

