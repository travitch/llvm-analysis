struct Inner {
  int *ip1;
  int *ip2;
};

struct Outer {
  int *op1;
  struct Inner i;
};

int a;
int b;
int *p;
int *q;
int **pp;

void setup(struct Outer *o) {
  o->i.ip1 = &b;
  o->i.ip2 = &a;
}

void target(struct Inner *i) {
  p = i->ip2;
}
