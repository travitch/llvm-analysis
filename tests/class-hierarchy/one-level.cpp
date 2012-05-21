class Base {
public:
  Base() {}
  virtual void f1() {}
};

class Derived : public Base {
public:
  Derived() : Base() {}
  virtual void f2() {}
};

Base * build() {
  return new Derived();
}
