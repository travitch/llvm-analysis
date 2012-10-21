#include <stdlib.h>

struct Foo;

struct Vtable {
  void (*close)(struct Foo*);
  void (*finish)(struct Foo*);
};

void (*target)(struct Foo*);

struct Foo {
  struct Vtable *vtbl;
  int * arr;
};

void userFinish(struct Foo* foo) {
  // foo->vtbl->finish(foo);
  target = foo->vtbl->finish;
}

void realFinish(struct Foo* foo) {
  free(foo->arr);
  free(foo);
}

void realClose(struct Foo* foo) {
  foo->arr[0] = 0;
}

struct Vtable* readVtable() {
  static struct Vtable vt;
  static int init = 0;

  if(!init) {
    vt.close = realClose;
    vt.finish = realFinish;
  }

  return &vt;
}

struct Foo* newFoo(int len) {
  struct Foo* foo = calloc(1, sizeof(struct Foo));
  foo->arr = calloc(len, sizeof(int));
  foo->vtbl = readVtable();
  return foo;
}
