/* { dg-do compile } */
/* { dg-options "-O2" } */

int cond (int a, int b) {
  return a > b;
}

/* { dg-final { scan-assembler "l.cmov" } } */
