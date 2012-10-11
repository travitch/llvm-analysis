int a;
int b;

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
  q = &a;
  q = &b;
  q = &arg;
}
