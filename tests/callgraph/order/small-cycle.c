void h()
{
}

void g();

void f()
{
  g();
  h();
}

void g()
{
  f();
}

int main(int argc, char *argv[])
{
  g();
  return 0;
}
