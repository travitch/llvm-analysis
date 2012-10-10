int a;
int c;

int ** store;
int*** arr;

int * p;
int * r;

void setup()
{
  p = &a;
  store[c] = p;
}

void target(int *** arg) {
  r = (*arg)[a];
}

void caller() {
  target(&store);
}
