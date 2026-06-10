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

int countdown = 1000000;

// Returns a struct whose .a reaches 0 after many iterations. If the condition
// temporary leaks stack space on each iteration, the stack overflows long
// before the loop finishes.
struct Pair next() {
  struct Pair p;
  countdown -= 1;
  p.a = countdown;
  p.b = 0;
  return p;
}

struct Pair make_pair(int a, int b) {
  struct Pair p;
  p.a = a;
  p.b = b;
  return p;
}

int main() {
  int n = 0;
  int i;

  // Struct-returning call in a while condition, many iterations
  while (next().a) {
    n += 1;
  }
  putintln(n);

  // Struct-returning call in a for condition
  n = 0;
  for (i = 0; make_pair(i, 0).a < 5; i += 1) {
    n += 10;
  }
  putintln(n);

  return 0;
}
