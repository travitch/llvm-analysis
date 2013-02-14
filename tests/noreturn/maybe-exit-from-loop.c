#include <stdlib.h>

int foo(void);

void f(int x) {
  while(x % 5) {
    x += foo();
    if(x % 7) exit(-1);
  }
}
