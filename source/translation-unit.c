#include <stdio.h>


struct b {
  void *foo;
};

void hej() {
  struct b apa = { NULL };
  printf("apa = %p\n", apa.foo);
}

  