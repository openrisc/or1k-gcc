/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-cunroll-details" } */
int a[2];
test(int c)
{ 
  int i;
  for (i=0;i<c;i++)
    {
      a[i]=5;
      if (test2())
	return;
    }
}
/* We are not able to get rid of the final conditional because the loop has two exits.  */
/* { dg-final { scan-tree-dump "Unrolled loop 1 completely .duplicated 1 times.." "cunroll"} } */
/* { dg-final { cleanup-tree-dump "cunroll" } } */
