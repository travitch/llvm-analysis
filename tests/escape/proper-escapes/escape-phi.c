extern int * g;
extern int rand();
void f(int * p) {
  int x;
  if(rand())
    g = p;
  else
    g = &x;
}
