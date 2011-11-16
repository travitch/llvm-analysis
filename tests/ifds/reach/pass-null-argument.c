#include <stdlib.h>

int y;
int *g = &y;

void f(int *p) {
  g = p;
}

int main() {
  y = 16;
  y *= 10;
  f(NULL);
  y = y / 4;
  return 0;
}
