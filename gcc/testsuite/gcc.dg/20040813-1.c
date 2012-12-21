/* Test lang in N_SO stab.  */
/* Contributed by Devang Patel  <dpatel@apple.com>  */

/* { dg-do compile } */
/* { dg-skip-if "No stabs" { aarch64*-*-* mmix-*-* *-*-aix* alpha*-*-* hppa*64*-*-* ia64-*-* tile*-*-* *-*-vxworks* } { "*" } { "" } } */
/* { dg-options "-gstabs" } */

int
main ()
{
  return 0;
}

/* { dg-final { scan-assembler ".stabs.*100,0,2" } } */

