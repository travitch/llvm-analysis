#include <stdlib.h>

typedef void (*fptr)(int**);

int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

fptr fp;

void callee1(int ** ptr1)
{
  *ptr1 = q;
}

void callee2(int ** ptr2)
{
  *ptr2 = r;
}

void apply(fptr f)
{
  f(&p);
}

void caller()
{
  apply(callee1);
}

void setup()
{
  q = &a;
  // r = &b;
//  pp = &p;
}
