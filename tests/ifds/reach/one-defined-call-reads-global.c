int *g;

void f(int *p) {
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
