#include <stdlib.h>

int y;
int *g = &y;
int *x = NULL;

void f(int *p) {
  g = p;
}

int main() {
  y = 16;
  y *= 10;
  f(x);
  y = y / 4;
  return 0;
}
