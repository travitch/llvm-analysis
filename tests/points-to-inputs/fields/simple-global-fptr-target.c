typedef void (*fptr)(int*);

struct S {
  int fld;
  fptr f;
};

fptr global;

void target(int* p) {

}

void setup(struct S *s) {
  s->f = target;
}

void query(struct S *s) {
  global = s->f;
}
