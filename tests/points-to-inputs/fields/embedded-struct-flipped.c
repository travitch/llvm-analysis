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

void setup(struct Inner *i) {
  i->ip2 = &b;

}

void target(struct Outer *o) {
  p = o->i.ip2;
}
