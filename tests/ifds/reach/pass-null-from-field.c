#include <stdlib.h>

int y;
int *g = &y;

int* arr[100];

void f(int *p) {
  g = p;
}

int main() {
  y = rand();
  /* y = 1; */
  /* y *= 10; */
  f(arr[y]);
  y = y / 4;
  return 0;
}
