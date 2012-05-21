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

class Derived2 : public Derived {
  int m_i;
public:
  Derived2(int i): m_i(i) {}

  virtual int f6() const { return m_i; }
};

Base * build() {
  return new Derived2(7);
}
