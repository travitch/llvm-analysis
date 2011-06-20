int a;
int b;
int c;

int * p;
int * q;
int ** pp;
int ** pq;

// Case 2
void copyPtr()
{
  p = q;
}

void setup(int arg)
{
  p = &c;
  q = &a;
  q = &b;
  q = &arg;
}
