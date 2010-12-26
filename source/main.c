/*
gcc -g -o main main.c && ./main
*/

#include <stdio.h>

int main (int argc, char **argv)
{
  float f = 1.12f;
  double d = 2.42;
  printf("f = %f, d = %f\n", f, d);

  return 0;
}
