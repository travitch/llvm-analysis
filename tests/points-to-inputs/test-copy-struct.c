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

void caller()
{
  global1.payload = p;
  global2 = global1;
}

void setup()
{
  p = &a;
  r = &b;
//  pp = &p;
}
