int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

void callee(int * ptr)
{
  *ptr = b;
}

void caller()
{
  callee(p);
}

void setup()
{
  q = &a;
  pq = &q;
}
