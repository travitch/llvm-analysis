#include <stdlib.h>

int y;
int *g = &y;

int arr[100];

int* f() {
  return NULL;
}

int main() {
  y = rand();
  /* y = 1; */
  /* y *= 10; */
  // f(&arr[y]);
  g = f();
  y = y / 4;
  return 0;
}
