extern int z;
int f(int x) {
  if(x > 10) {
    z += x;
    return 10;
  }

  return -79;
}
