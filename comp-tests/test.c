#include <stdio.h>

#define NL 10
#define ZERO '0'

void main() {

  /* print a number to stdout */

  int n = 31416;
  int p = 1;

  while (p * 10 <= n) p *= 10;

  while (p > 0) {
    int digit = n / p;
    putchar(ZERO + digit);
    n %= p;
    p /= 10;
  }

  putchar(NL);
}
