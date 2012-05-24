struct DispatchBase {
  DispatchBase();
  virtual void f();
};

DispatchBase::DispatchBase() {
}

void DispatchBase::f() {

}

void test(DispatchBase * b) {
  b->f();
}
