int a;
int b;
int c;
int d;

int * p;
int * q;
int * r;
int ** pq;
int ** pp;

extern int rand(void);

void setup()
{
  int * z;
  if(rand() > 0) {
    z = &a;
  }
  else {
    z = &b;
  }

  p = z;
}
