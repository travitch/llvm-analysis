extern int z;
int f(int x) {
  if(x < 10) {
    z += x;
    return 10;
  }

  for(int i = 0; i < x; ++i) {
    z += i - x;
  }

  return -79;
}
