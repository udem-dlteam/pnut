// tests for #include "" directives
// pnut_opt: -Iportable_libc/include/

#include <stdio.h>

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
  putint(stdin);
  putint(stdout);
  putint(stderr);
}
