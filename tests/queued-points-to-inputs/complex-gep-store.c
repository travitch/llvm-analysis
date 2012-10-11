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

struct S
{
  struct S * next;
  int* payload;
};

struct S global;

void caller()
{
  global.payload = p;
}

void setup()
{
  p = &a;
  r = &b;
//  pp = &p;
}
