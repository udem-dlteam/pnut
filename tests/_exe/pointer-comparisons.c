#include <stdio.h>

void putint(int n) {
  if (n < 0) {
    putchar('-');
    n = -n;
  }
  if (n >= 10) {
    putint(n / 10);
  }
  putchar('0' + n % 10);
}

int *fun() {
  return 0;
}

void main() {
  char c = 15;
  c = (fun() != 0) ^ c;
  putint(c); putchar('\n');
  c = (fun() != 1) ^ c;
  putint(c); putchar('\n');
}
