int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;

// Case 3
void copyPtr()
{
  p = *pq;
}

void setup()
{
  q = &a;
  q = &b;
  r = &c;
  pq = &r;
  pq = &q;
}
