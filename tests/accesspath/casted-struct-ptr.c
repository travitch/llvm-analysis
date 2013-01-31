struct S {
  int x;
  int y;
};

struct T {
  void *s;
  int x;
};

void f(struct T *t) {
  struct S *s = t->s;
  s->y = 10;
}
