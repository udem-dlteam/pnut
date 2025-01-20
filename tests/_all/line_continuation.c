#include <stdio.h>

void putint_aux(int n, int base) {
  int d = n % base;
  int top = n / base;
  if (n == 0) return;
  putint_aux(top, base);
  putchar("0123456789abcdef"[d & 15]);
}

void putint(int n, int base) {
  if (n < 0) {
    putchar('-');
    putint_aux(-n, base);
  } else {
    putint_aux(n, base);
  }
}

void main() {

/**/
int foo = 0;

/\
*
*/ fo\
o +\
= 0\
x\
10\
200;

    putint(foo, 10);
    return 0;
}