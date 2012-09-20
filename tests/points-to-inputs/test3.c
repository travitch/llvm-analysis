int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

int ** ptr;
void callee()
{
  // *ptr = q;
  *pp = q;
}

void caller()
{
  //p = &d;
  // ptr = pp;
}

void setup()
{
  q = &a;
  pp = &p;
}
