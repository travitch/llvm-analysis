namespace ns1 {
  class Base {
  public:
    Base() {}
    virtual void f1();
  };


void Base::f1 () {

}
}
namespace ns2 {
  class Base {
  public:
    Base() {}
    virtual void f1();
  };


void Base::f1 () {

}
}

namespace m {
  class Middle1 : public ns1::Base {
  public:
    virtual void m1() {}
    virtual void f1();
  };

  class Middle2 : public ns2::Base {
  public:
    Middle2();
    virtual void m2() const {}
  };


  Middle2::Middle2() {

  }

  void Middle1::f1() {

  }
}


class Derived : public m::Middle1, public m::Middle2 {
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



m::Middle2* mm() {
  m::Middle2 * p = new m::Middle2();
  p->m2();
  return p;
}
