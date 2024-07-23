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

void main() {
  int n;
  int i = 0;
  while (i < 20) {
    n = fib(i);
    printf("fib(%d) = %d\n", i, n);
    ++i;
  }
}
