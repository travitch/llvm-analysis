void f(int * p, void(*func)(int*)) {
  func(p);
}
