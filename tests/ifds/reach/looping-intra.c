int *g;
int y;
int main() {
  int i = 0;
  y = 16;
  y *= 10;
  for(i = 0; i < 100; ++i) {
    if(y < 0) {
      y = y / 4;
    }
    else {
      y = y % 3;
    }
  }
  y = y & 5;
  return 0;
}
