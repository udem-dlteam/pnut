/*
 * fib.c: Print the first 20 Fibonacci numbers.
 *
 * Usage: ./fib.sh
 */

#include <stdio.h>

int fib(int n) {
  if (n < 2) {
    return n;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

#ifndef PNUT_SH
void putint(int n) {
  if (n >= 10) {
    putint(n / 10);
  }
  putchar(n % 10 + '0');
}

void putstr(const char *s) {
  while (*s) {
    putchar(*s++);
  }
}
#endif

void main() {
  int n;
  int i = 0;
  while (i < 20) {
    n = fib(i);
#ifdef PNUT_SH
    printf("fib(%d) = %d\n", i, n);
#else
    putstr("fib(");
    putint(i);
    putstr(") = ");
    putint(n);
    putstr("\n");
#endif
    ++i;
  }
}
