int g;
int * h() {
  return &g;
}

int *f() {
  return h();
}
