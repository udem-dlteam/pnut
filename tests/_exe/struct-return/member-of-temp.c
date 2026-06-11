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
}

void putintln(int n) {
  putint(n);
  putchar('\n');
}

struct Pair {
  int a;
  int b;
};

struct Nested {
  struct Pair p;
  int c;
};

struct Pair make_pair(int a, int b) {
  struct Pair p;
  p.a = a;
  p.b = b;
  return p;
}

struct Nested make_nested(int a, int b, int c) {
  struct Nested n;
  n.p.a = a;
  n.p.b = b;
  n.c = c;
  return n;
}

int main() {
  int x = 100;

  // Member access on a temporary
  putintln(make_pair(5, 6).a);
  putintln(make_pair(5, 6).b);

  // Temporary member access nested in arithmetic
  putintln(x + make_pair(7, 8).a * 2 + make_pair(1, 2).b);

  // Member of an aggregate member of a temporary
  putintln(make_nested(3, 4, 5).p.b);
  putintln(make_nested(3, 4, 5).c);

  // Temporary member access in a condition
  if (make_pair(1, 0).a) {
    putintln(111);
  }
  if (make_pair(1, 0).b) {
    putintln(222);
  }

  return 0;
}
