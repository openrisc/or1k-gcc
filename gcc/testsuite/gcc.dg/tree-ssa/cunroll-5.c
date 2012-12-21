/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-cunroll-details" } */
int *a;
test(int c)
{ 
  int i;
  for (i=0;i<6;i++)
    a[i]=5;
}
/* Basic testcase for complette unrolling.  */
/* { dg-final { scan-tree-dump "Unrolled loop 1 completely .duplicated 5 times.." "cunroll"} } */
/* { dg-final { scan-tree-dump "Exit condition of peeled iterations was eliminated." "cunroll"} } */
/* { dg-final { scan-tree-dump "Last iteration exit edge was proved true." "cunroll"} } */
/* { dg-final { cleanup-tree-dump "cunroll" } } */
