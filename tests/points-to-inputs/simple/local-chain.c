int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

void caller()
{
  int *** x = &pq;
  int ** y = *x;
  *y = *pp;
  *y = r;
}

void setup()
{
  r = &c;
  p = &b;
  q = &a;
  pq = &q;
  pp = &p;
}
