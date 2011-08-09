int *g;
int y;
int main() {
  y = 16;
  y *= 10;
  if(y < 0) {
    y = y / 4;
  }
  else {
    y = y % 3;
  }
  y = y & 5;
  return 0;
}
