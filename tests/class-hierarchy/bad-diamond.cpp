class Base {
public:
  Base() {}
  virtual void f1() {}
};

class Middle1 : public Base {
public:
  virtual void m1() {}
};

class Middle2 : public Base {
public:
  virtual void m2() const {}
};

class Derived : public Middle1, public Middle2 {
  int m_i;
public:
  Derived(int);
  virtual int f3() const;
};

Derived::Derived(int i): m_i(i) {}
int Derived::f3() const {
  this->m2();
  return m_i;
}
