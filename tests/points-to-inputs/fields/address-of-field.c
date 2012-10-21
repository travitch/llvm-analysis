struct S {
  int *fld;
};

int a;
int b;
int *target;
int **pp;

void setup(struct S* s) {
  s->fld = &a;
}

void setup2(struct S* s) {
  // This introduces an extra points-to relation in the result: @pp ->
  // %1, since pp points to this unnamed field.  Not a problem and
  // actually correct, just a bit odd in the expected result.
  pp = &s->fld;
}

void extract(struct S* s) {
  target = *pp;
}
