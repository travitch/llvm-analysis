extern int g(int);
extern int z;
int f(int x) {

  while(x > 10) {
    if(g(x) < 0) return -1;

    z += x;
    return 10;
  }

  for(int i = 0; i < x; ++i) {
    z += i - x;
  }

  return -79;
}
