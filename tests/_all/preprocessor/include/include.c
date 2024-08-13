// tests for #include "" directives

// putchar
#include <stdio.h>

#define INCLUDE
#include "include1.h"
// #include "include2.h"
// #include "include3.h"
// #include "include4.h"

void putint_aux(int n) {
  if (n <= -10) putint_aux(n / 10);
  putchar('0' - (n % 10));
}

void putint(int n) {
  if (n < 0) {
    putchar('-');
    putint_aux(n);
  } else {
    putint_aux(-n);
  }

  putchar('\n');
}

void main() {
  putint(CONSTANT1);
  putint(CONSTANT2);
  putint(CONSTANT3);
  putint(CONSTANT4);
  putint(CONSTANT5);
  putint(CONSTANT6);
  putint(CONSTANT7);
  putint(CONSTANT8);
  putint(CONSTANT9);
}
