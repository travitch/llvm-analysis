/*
  Note, this test is important because p picks up two edges from q at
  the same step of the algorithm.  This gives us a chance to catch a
  potential regression in graph construction (due to context
  clobbering in FGL)
 */
int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

void copyDoubleIndirectPtr()
{
  *pq = *pp;
}

void setup()
{
  q = &a;
  q = &b;
  r = &c;
  p = &d;
  pp = &p;
  pq = &r;
  pq = &q;
}
