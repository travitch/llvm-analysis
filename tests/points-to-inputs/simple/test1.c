int z;
int a;
int b;
int * p = &a;
int * q = &z;

int ** pp;
int ** pq;

void copyPtr() {
  p = q;
}
