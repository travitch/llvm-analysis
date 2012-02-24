extern int * glob;
void g(int * p, int * b) {
  glob = p;
}

void f(int * q, int a) {
  g(q, &a);
}
