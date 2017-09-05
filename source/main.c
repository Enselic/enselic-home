/*
gcc -msse2 -g -o main main.c && ./main
gcc -Wstrict-overflow -fno-strict-aliasing -Werror -Wshadow -Wall -Wextra -Wpedantic -std=c99 -g -lm $(pkg-config glib-2.0 --cflags --libs) -o main main.c && ./main
*/

#include <math.h>
#include <glib.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

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

  return 0;
}
