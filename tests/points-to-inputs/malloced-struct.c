#include <stdlib.h>

int a;
int b;
int c;
int d;

int* arr[10];

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

struct S {
  int * payload;
};

struct S global;
struct S* globalPtr = &global;

void caller(int x)
{
  globalPtr = malloc(sizeof(struct S));
  globalPtr->payload = p;
}

void setup()
{
  p = &a;
//  r = &b;
//  pp = &p;
}
