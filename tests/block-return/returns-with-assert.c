#include <assert.h>

extern int z;

int f(int x) {

  assert(x < 10000);

  if(x > 10) {
    z += x;
    return 10;
  }

  return -79;
}
