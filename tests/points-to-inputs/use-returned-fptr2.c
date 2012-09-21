int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

typedef void (*fptr)(int**);

void callee1(int ** ptr1)
{
  *ptr1 = q;
}

void callee2(int ** ptr2)
{
  *ptr2 = r;
}

extern int rand();

fptr funcFactory()
{
  if(rand() > 0) {
    return callee2;
  }

  return callee1;
}

void caller()
{
  fptr f = funcFactory();
  f(pp);
}

void setup()
{
  q = &a;
  r = &b;
  pp = &p;
  // fptr = &callee1;
}
