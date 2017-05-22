/*
gcc -g -o main main.c translation-unit.c && ./main
*/

#include <stdio.h>

struct b {
  int a;
};

extern void hej();
int main (int argc, char **argv)
{
  struct b f_inst = { 1 };
  float f = 1.12f;
  double d = 2.42;
  printf("f = %f, d = %d\n", f, f_inst.a);

  hej();

  return 0;
}
