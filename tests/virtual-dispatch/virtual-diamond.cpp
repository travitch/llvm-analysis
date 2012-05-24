class Base {
public:
  Base() {}
  virtual void f1();
};

class Middle1 : public virtual Base {
public:
  virtual void m1() {}
  virtual void f1();
};

class Middle2 : public virtual Base {
public:
  Middle2();
  virtual void m2() const {}
};


class Derived : public Middle1, public Middle2 {
  int m_i;
public:
  Derived(int);
  virtual int f3() const;
  virtual void f1();
};

Derived::Derived(int i): m_i(i) {}
int Derived::f3() const {
//  this->m2();
  return m_i;
}



Middle2::Middle2() {

}

void Base::f1 () {

}

void Middle1::f1() {

}

void Derived::f1() {

}

void mm(Base * b) {
  b->f1();
}
