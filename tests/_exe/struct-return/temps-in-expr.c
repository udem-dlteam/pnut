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

// Asymmetric in x/y: detects swapped argument order
struct Pair sub_pairs(struct Pair x, struct Pair y) {
  struct Pair diff;
  diff.a = x.a - y.a;
  diff.b = x.b - y.b;
  return diff;
}

int main() {
  struct Pair s;

  // Two temporaries in one binary expression
  putintln(make_pair(1, 2).a + make_pair(30, 40).b);

  // Non-commutative op: operand order must be preserved
  putintln(make_pair(5, 6).b - make_pair(1, 2).a);
  putintln(make_pair(100, 0).a / make_pair(4, 0).a);

  // Both arguments of a call are themselves call temporaries
  s = add_pairs(make_pair(1, 2), make_pair(10, 20));
  putintln(s.a);
  putintln(s.b);

  // Same, with an argument-order-sensitive function
  s = sub_pairs(make_pair(50, 60), make_pair(8, 9));
  putintln(s.a);
  putintln(s.b);

  // Three temporaries in one expression
  putintln(make_pair(1, 0).a + make_pair(2, 0).a + make_pair(3, 0).a);

  return 0;
}
