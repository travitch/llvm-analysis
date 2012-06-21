#include <stdlib.h>

struct Vtbl {
  void* (*malloc_ptr)(size_t);
  void (*free_ptr)(void*);
  void* (*realloc_ptr)(void*, size_t);
  void* (*calloc_ptr)(size_t, size_t);
};

void f(struct Vtbl *vtbl) {
  vtbl->calloc_ptr = calloc;
}
