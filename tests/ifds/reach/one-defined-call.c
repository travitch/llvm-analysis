void f(int *p) {
  // The transfer functions aren't robust enough to associate this
  // back to g
  *p = 5;
}

int *g;
int y;

int main() {
  y = 16;
  y *= 10;
  f(g);
  y = y / 4;
  return 0;
}
