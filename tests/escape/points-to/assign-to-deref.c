extern int a;
extern int b;
extern int c;
int * p;
int ** pp;
int * q;
int ** pq;

void f() {
  int *** x;
  int ** y;
  int * r = &c;

  p = &a;
  q = &b;
  pp = &p;
  pq = &q;
  x = &pq;
  y = *x;
  // In this loop, *y is q.  Since q is global, the edges from it are
  // *not* killed and it can point to a, b, or c at the end of the
  // function.
  if(a > 10) {
    *y = *pp;
  }
  else {
    *y = r;
  }
}
