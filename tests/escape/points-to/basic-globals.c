int a;
int * p;
int ** pp;

void f() {
  pp = &p;
  p = &a;
}
