typedef void (*fptr)(int*, int**);

struct S {
  int** fld;
  fptr f;
};

int a;
int* storage;

void target(int *p, int **back) {
  *back = p;
}

void setup(struct S *s) {
  s->f = target;
}

void query(struct S *s) {
  s->f(&a, &storage);
}
