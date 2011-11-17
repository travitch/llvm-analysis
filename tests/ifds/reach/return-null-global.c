#include <stdlib.h>

int y;
int *h = NULL;
int *g = &y;

int arr[100];

int* f() {
  return h;
}

int main() {
  /* y = 1; */
  /* y *= 10; */
  // f(&arr[y]);
  g = f();
  y = y / 4;
  return 0;
}
