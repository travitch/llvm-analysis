#include <stdlib.h>

int y;
int *g = &y;
int *h;

int arr[100];

int* f() {
  int * x;
  x = h;
  return x;
}

int main() {
  /* y = 1; */
  /* y *= 10; */
  // f(&arr[y]);
  g = f();
//  y = y / 4;
  return 0;
}
