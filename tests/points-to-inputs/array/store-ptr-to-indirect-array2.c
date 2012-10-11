int a;
int c;

int ** store;
int*** arr;

int * p;
int * r;

void setup()
{
  arr = &store;
  p = &a;
  store[c] = p;
}

void target() {
  r = (*arr)[a];
}
