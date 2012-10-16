typedef void (*fptr)(int*);

struct S {
  int** fld;
  fptr f;
};

struct T {
  int ** fld;
};

int a;
int* storage;
int **target;

void setup(struct S *s) {
  s->fld = &storage;
}

void query(struct S *s, struct T *t) {
  t->fld = s->fld;
}

void call(struct T* t) {
  target = t->fld;
}
