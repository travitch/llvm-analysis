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


void caller(int x)
{
  p = malloc(100);
}

void setup()
{
  p = &a;
//  r = &b;
//  pp = &p;
}
