#include <stdio.h>

int *g;
int y;
int main() {
  y = 16;
  y *= 10;
  printf("%p", g);
  y = y / 4;
  return 0;
}
