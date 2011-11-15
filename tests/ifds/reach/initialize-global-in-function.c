int y;
int *g;

void f(int *p) {
  g = p;
}

int main() {
  y = 16;
  y *= 10;
  f(&y);
  y = y / 4;
  return 0;
}
