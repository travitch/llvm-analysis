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

struct OuterStruct {
  struct S s1;
  struct S s2;
};


struct OuterStruct global1;
struct OuterStruct global2;

struct S global3;

void caller(int x)
{
  global3.payload = p;
  global1.s1 = global3;
}

void setup()
{
  p = &a;
  r = &b;
//  pp = &p;
}
