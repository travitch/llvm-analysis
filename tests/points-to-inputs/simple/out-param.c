int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

void callee(int ** ptr)
{
  *ptr = q;
}

void caller()
{
  // Proxy some points-to information through this local
  int * px;
  callee(&px);
  p = px;
}

void setup()
{
  q = &a;
  pq = &q;
}
