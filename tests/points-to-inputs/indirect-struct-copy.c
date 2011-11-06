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

struct S global1;
struct S global2;
struct S global3;

struct S * globalPtr;

void caller(int x)
{
  if(x)
    globalPtr = &global2;
  else
    globalPtr = &global3;

  global1.payload = p;
  *globalPtr = global1;
}

void setup()
{
  p = &a;
  r = &b;
//  pp = &p;
}
