struct DispatchBase {
  DispatchBase();
  virtual void f();
  virtual void g();
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

void DispatchBase::g() {

}

void test(DispatchBase * b) {
  b->g();
}
