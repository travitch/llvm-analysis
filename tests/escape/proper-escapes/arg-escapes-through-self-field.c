struct S
{
  int * sP;
  int * sQ;
  struct S *next;
};

void f(struct S * s) {
  struct S * s0;

  s0 = s->next;
  s0->next = s;
}
