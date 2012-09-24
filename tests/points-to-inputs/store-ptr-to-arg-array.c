int a;
int c;

int** arr;

int * p;
int * r;

void callee(int ** arg) {
  r = arg[c];
}

void setup()
{
  p = &a;
  arr[c] = p;
//  r = arr[a];
  callee(arr);
}
