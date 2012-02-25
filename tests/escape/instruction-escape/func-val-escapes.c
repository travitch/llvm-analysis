int g;
int * h() {
  return &g;
}

int * p;

void f() {
  p = h();
}
