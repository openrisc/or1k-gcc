/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-store" } */

#define N 1024

char **ep;
char **fp;

void
avx_test (void)
{
  int i;
  char **ap;
  char **bp;
  char **cp;

  ap = ep;
  bp = fp;
  for (i = 128; i >= 0; i--)
    {
      *ap++ = *cp++;
      *bp++ = 0;
    }
}

/* { dg-final { scan-assembler-not "avx_storedqu256" } } */
/* { dg-final { scan-assembler "vmovdqu.*\\*movv16qi_internal/3" } } */
/* { dg-final { scan-assembler "vextract.128" } } */
