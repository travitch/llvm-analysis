int a;
int b;
int c;
int d;

int* arr[10];

int ** ary;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;


void callee()
{
  pp = &arr[c];
  p = *pp;
  // p should be able to point to either a or b
}

void setup()
{
  pp = &p;
  q = &a;
  r = &b;
  arr[a] = q;
  arr[b] = r;
  callee();
}
