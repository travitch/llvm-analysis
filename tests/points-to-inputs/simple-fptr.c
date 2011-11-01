int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

void (*fptr)(int**);

void callee1(int ** ptr1)
{
  *ptr1 = q;
}

void callee2(int ** ptr2)
{
  *ptr2 = r;
}

void caller()
{
  fptr(pp);
}

void setup()
{
  q = &a;
  r = &b;
  pp = &p;
  fptr = &callee1;
}
