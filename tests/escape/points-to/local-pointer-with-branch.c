/*
  This test involves writes to a global.  These edges should not be
  killed when new writes are recorded.
 */
extern int a;
int ** pp;

void f() {
  int z;
  int * p;
  if(a > 10) {
    p = &a;
  }
  else {
    p = &z;
  }
}
