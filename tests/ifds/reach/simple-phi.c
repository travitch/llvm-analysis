
#include <stdlib.h>

int y;
int z;
int *g;
int *h;

/* int arr[100]; */

/* int* f() { */
/*   return &arr[3]; */
/* } */

int main() {
  int * x;
  if(rand() > 4)
    x = &y;
  /* else */
  /*   x = &z; */
  /* y = 1; */
  /* y *= 10; */
  // f(&arr[y]);
  g = x; // f();
//  y = y / 4;
  return 0;
}
