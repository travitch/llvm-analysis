typedef void (*fptr)(int*);

struct S {
  int** fld;
  fptr f;
};

int a;
int* storage;

void setup(struct S *s) {
  s->fld = &storage;
}

void query(struct S *s) {
  *s->fld = &a;
}
