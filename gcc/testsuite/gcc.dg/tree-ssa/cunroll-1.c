/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-cunrolli-details" } */
int a[2];
test(int c)
{ 
  int i;
  for (i=0;i<c;i++)
    a[i]=5;
}
/* Array bounds says the loop will not roll much.  */
/* { dg-final { scan-tree-dump "Unrolled loop 1 completely .duplicated 2 times.." "cunrolli"} } */
/* { dg-final { scan-tree-dump "Last iteration exit edge was proved true." "cunrolli"} } */
/* { dg-final { cleanup-tree-dump "cunrolli" } } */
