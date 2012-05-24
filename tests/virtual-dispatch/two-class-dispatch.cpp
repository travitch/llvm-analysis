struct DispatchBase {
  DispatchBase();
  virtual void f();
};

struct Derived : public DispatchBase {
  Derived();
  virtual void f();
};

Derived::Derived(): DispatchBase() {
}

DispatchBase::DispatchBase() {
}

void DispatchBase::f() {

}

void Derived::f() {

}

void test(DispatchBase * b) {
  b->f();
}
