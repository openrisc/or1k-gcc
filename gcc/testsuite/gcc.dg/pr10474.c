/* { dg-do compile } */
/* { dg-options "-O3 -fdump-rtl-pro_and_epilogue"  } */

void f(int *i)
{
	if (!i)
		return;
	else
	{
		__builtin_printf("Hi");
		*i=0;
	}
}

/* { dg-final { scan-rtl-dump "Performing shrink-wrapping" "pro_and_epilogue"  } } */
/* { dg-final { cleanup-rtl-dump "pro_and_epilogue" } } */
