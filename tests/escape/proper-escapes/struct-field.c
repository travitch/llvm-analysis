struct S {
  int * sP;
  int * sQ;
};

extern int * g;

void f(struct S * s) {
  g = s->sP;
}
