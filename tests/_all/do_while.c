#include <stdio.h>

int factorial(int num) {
  int ret = 1;

  do {
    ret *= num;
    num--;
  } while(num > 0);

  return ret;
}

void main() {
  int fac5 = factorial(5);
  int fac10 = factorial(10);

  if (fac5 == 120) {
    putchar('O');
  } else {
    putchar('B');
  }

  if (fac10 == 3628800) {
    putchar('O');
  } else {
    putchar('B');
  }
}
