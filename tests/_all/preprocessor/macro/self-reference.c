// tests for recursion depth of macros

// putchar
#include <stdio.h>

int test1 = 1;
int test2 = 2;
int test3 = 3;
int x = 4;
int y = 5;

void putdigit(int n) {
  putchar('0' + n);
  putchar('\n');
}

void A(int a, int b) {
  putdigit(a);
  putdigit(b);
}

void B(int a, int b) {
  putdigit(a);
  putdigit(b);
}

#define test1 test1
#define test2 test3
#define test3 test2
#define x 1 + y
#define y 1 + x
#define A(a1, a2) A(a1, a2)
#define B(a1, a2) C(a1, a2)
#define C(a1, a2) B(a1, a2)

void main() {
  putdigit(test1);
  putdigit(test2);
  putdigit(test3);
  putdigit(x);
  putdigit(y);
  A(test1, test2);
  A(test3, x);
  // B(x, y);
}
