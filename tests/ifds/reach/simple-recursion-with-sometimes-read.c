int *g;

void f(int *p) {
  if(*p > 0) {
    --(*p);
    f(p);
  }
  // This can only be reached sometimes.
  if(*p == -10)
    *p = *g;
}

int y;

int main() {
  y = 16;
  y *= 10;
  f(&y);
  y = y / 4;
  return 0;
}
