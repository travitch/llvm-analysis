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
  p = ary[c]; // arr[c] = b;
}

void setup()
{
  q = &a;
  r = &b;
  arr[a] = q;
  arr[b] = r;
  ary = arr;
  callee();
//  pp = &p;
}
