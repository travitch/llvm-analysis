struct S {
  int x;
};

extern struct S *s;

void f() {
  s->x = 5;
}
