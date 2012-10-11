int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

int** callee()
{
  return pq;
}

void caller()
{
  int** ret = callee();
  *ret = p;
}

void setup()
{
  p = &b;
  q = &a;
  pq = &q;
}
