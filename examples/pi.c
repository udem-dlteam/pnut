#ifndef PNUT_CC
#include <stdio.h>
#endif

/* https://cs.uwaterloo.ca/~alopez-o/math-faq/mathtext/node12.html */

int r[2801];

int main() {

  int i;
  int k;
  int b;
  int d;
  int c = 0;

  i = 0;

  while (i < 2800) {
    r[i] = 2000;
    i = i+1;
  }

  r[i] = 0;

  k = 2800;

  while (k > 0) {

    d = 0;

    i = k;
    while (i > 0) {
      d = d * i;
      d = d + r[i] * 10000;
      b = 2 * i - 1;

      r[i] = d % b;
      d = d / b;
      i = i-1;
    }

    putchar(48 + (c + d / 10000) / 1000 % 10);
    putchar(48 + (c + d / 10000) / 100 % 10);
    putchar(48 + (c + d / 10000) / 10 % 10);
    putchar(48 + (c + d / 10000) % 10);
    c = d % 10000;

    k = k - 14;
  }

  putchar(10);

  return 0;
}
