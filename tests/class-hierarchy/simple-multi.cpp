class Base1 {
public:
  Base1() {}
  virtual void f1() {}
};

class Base2 {
public:
  Base2() {}
  virtual void f2() {}
};

class Derived : public Base1, public Base2 {
  int m_i;
public:
  Derived(int);
  virtual int f3() const;
};

Derived::Derived(int i): m_i(i) {}
int Derived::f3() const {
  return m_i;
}
