int a;
int b;
int c;
int d;

int* arr[10];

int * p;
int * q;
int * r;
int ** pq;
int ** pp;


void callee(int *ary[10])
{
  p = ary[c]; // arr[c] = b;
}

void setup()
{
  q = &a;
  r = &b;
  arr[a] = q;
  arr[b] = r;
  callee(arr);
//  pp = &p;
}
