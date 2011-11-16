#include <stdlib.h>

int y;
int *g = &y;

int* arr[10];

void f(int *p) {
  g = arr[*p];
}

int main() {
  y = 16;
  y *= 10;
  f(NULL);
  y = y / 4;
  return 0;
}
