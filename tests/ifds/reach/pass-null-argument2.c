#include <stdlib.h>

int y;
int *g = &y;

void f(int *p) {
  g = p;
}

int main() {
  int * x = NULL;
  y = 16;
  y *= 10;
  f(x);
  y = y / 4;
  return 0;
}
