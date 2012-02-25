int g;
int * y;
int * h(int * p) {
  y = p;
  return &g;
}

int *f() {
  int * z = &g;
  for(int i = 0; i < 10; ++i) {
    z = h(z);
  }
}
