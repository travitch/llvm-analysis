struct S
{
  int * sP;
  int * sQ;
};

extern struct S* getLatestS();

void f(int * p) {
  struct S *s = getLatestS();
  s->sQ = p;
}
