typedef void(*fptrty)(int**);

extern int a;
extern int b;
extern int c;
int *backing;
fptrty * arr; // [10];

void f1(int **store) {
  *store = &a;
}

void f2(int **store) {
  *store = &b;
}

void f3(int **store) {
  *store = &c;
}

void setup() {
  arr[a] = f1;
  arr[c] = f3;
}

void call() {
  arr[b](&backing);
}
