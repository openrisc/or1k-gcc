/* { dg-do run } */
/* { dg-options "-O2 -msoft-div -msoft-mul" } */

struct testcase {
  int a;
  int b;
  int c;
  int expected;
};

struct testcase tests[] = {
  {2, 200, 3, 133},
  {3, 300, 3, 300},
  {2, 500, 3, 333},
  {4, 250, 3, 333},
  {0, 0, 0, 0}
};

int calc (int a, int b, int c) {
  return a * b / c;
}

int main () {
  int fail = 0;
  struct testcase *tc;

  for (int i = 0; (tc = &tests[i], tc->c); i++)
    fail |= (calc (tc->a, tc->b, tc->c) != tc->expected);

  return fail;
}
