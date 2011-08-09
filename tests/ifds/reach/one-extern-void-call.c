void f(int *);

int *g;
int y;
int main() {
  y = 16;
  y *= 10;
  f(g);
  y = y / 4;
  return 0;
}
