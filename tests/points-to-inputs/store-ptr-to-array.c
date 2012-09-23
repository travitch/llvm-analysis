int a;
int c;

int** arr;

int * p;
int * r;

void setup()
{
  p = &a;
  r = arr[a];
  arr[c] = p;
}
