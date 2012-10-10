int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

extern void f(int **);

void setup() {
  p = &a;
  pp = &p;
  f(pp);
}
