/*
  This test involves writes to a global.  These edges should not be
  killed when new writes are recorded.
 */
int a;
int * p;
int ** pp;

void f() {
  int z;
  p = &a;
  pp = &p;
  p = &z;
}
