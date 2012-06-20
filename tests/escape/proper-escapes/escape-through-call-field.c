struct S {
  int * sP;
  int * sQ;
};

extern int * g;

void fldEsc(struct S * s) {
  g = s->sP;
}

void f(int * p) {
  struct S s;
  s.sP = p;
  fldEsc(&s);
}
