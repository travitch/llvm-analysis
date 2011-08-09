int *g;
int* f() {
  return g;
}

int main() {
  int y;
  int * p = f(); // FIXME: transfer function for store is wrong here
  y += 7;
//  y = *p & 10;

  return 0;
}
