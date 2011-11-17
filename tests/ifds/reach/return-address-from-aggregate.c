#include <stdlib.h>

int y;
int *g;
int *h;

int arr[100];

int* f() {
  return &arr[3];
}

int main() {
  /* y = 1; */
  /* y *= 10; */
  // f(&arr[y]);
  g = f();
//  y = y / 4;
  return 0;
}
