/*
  This test involves writes to a global.  These edges should not be
  killed when new writes are recorded.
 */
extern int a;
int ** pp;

void f() {
  int z;
  int g;
  int * p;
  p = &g;
  while(a) {
    p = &z;
    --a;
  }
}
