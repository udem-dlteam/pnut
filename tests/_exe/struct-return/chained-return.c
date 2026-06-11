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

struct Pair make_pair(int a, int b) {
  struct Pair p;
  p.a = a;
  p.b = b;
  return p;
}

struct Pair add_pairs(struct Pair x, struct Pair y) {
  struct Pair sum;
  sum.a = x.a + y.a;
  sum.b = x.b + y.b;
  return sum;
}

// Return value of another struct-returning call
struct Pair forward(int a, int b) {
  return make_pair(a, b);
}

// Chained calls in the return expression
struct Pair add3(struct Pair x, struct Pair y, struct Pair z) {
  return add_pairs(add_pairs(x, y), z);
}

// Return a by-value parameter
struct Pair identity(struct Pair p) {
  return p;
}

int main() {
  struct Pair s;

  s = forward(13, 14);
  putintln(s.a);
  putintln(s.b);

  s = add3(make_pair(1, 2), make_pair(10, 20), make_pair(100, 200));
  putintln(s.a);
  putintln(s.b);

  s = identity(make_pair(-5, -7));
  putintln(s.a);
  putintln(s.b);

  return 0;
}
