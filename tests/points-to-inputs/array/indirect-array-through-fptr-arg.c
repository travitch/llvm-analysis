int a;
int c;

int ** store;
int*** arr;

int * p;
int * q;
int * r;

typedef void (*fptr)(int**, int ***);

fptr gfp;

void target1(int ** theptr, int *** arg) {
  *theptr = (*arg)[a];
}

void target2(int *** arg2) {
  q = (*arg2)[a];
}

void setup()
{
  p = &a;
  store[c] = p;
  gfp = target1;
}

void caller() {
  gfp(&r, &store);
}
