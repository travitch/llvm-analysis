extern int * p;

void f() {
  int z;
  int * y = &z;
  p = y;
}
