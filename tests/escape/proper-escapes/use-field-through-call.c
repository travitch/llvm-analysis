#include <stdio.h>

struct S {
  int * sP;
  const char * sQ;
};

void f(struct S * s) {
  puts(s->sQ);
}

void g(const char * str) {
  struct S s;
  int x = 6;
  s.sQ = str;
  s.sP = &x;
  f(&s);
}
