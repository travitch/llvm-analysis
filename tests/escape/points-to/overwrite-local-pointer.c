/*
  This test involves writes to a global.  These edges should not be
  killed when new writes are recorded.
 */
int a;
int ** pp;

void f() {
  int z;
  int * p;
  p = &a;
  p = &z;
}
