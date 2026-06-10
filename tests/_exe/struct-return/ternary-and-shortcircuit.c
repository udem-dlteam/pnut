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

int call_count = 0;

struct Pair make_pair(int a, int b) {
  struct Pair p;
  call_count += 1;
  p.a = a;
  p.b = b;
  return p;
}

void print_pair(struct Pair p) {
  putintln(p.a);
  putintln(p.b);
}

int main() {
  struct Pair fallback;
  struct Pair s;
  int yes = 1;
  int no = 0;

  fallback.a = 77;
  fallback.b = 88;

  // Struct-valued ternary: call arm taken
  s = yes ? make_pair(1, 2) : fallback;
  putintln(s.a);
  putintln(s.b);

  // Struct-valued ternary: lvalue arm taken, call must not run
  s = no ? make_pair(1, 2) : fallback;
  putintln(s.a);
  putintln(s.b);
  putintln(call_count);

  // Member access on a struct-valued ternary
  putintln((yes ? make_pair(9, 10) : fallback).b);

  // Struct-valued ternary passed directly as a by-value argument
  print_pair(yes ? make_pair(20, 21) : fallback);
  print_pair(no ? make_pair(20, 21) : fallback);

  // Scalar-valued short-circuit: temp allocated in one branch only.
  putintln(yes && make_pair(0, 9).b);
  putintln(no && make_pair(0, 9).b);
  putintln(no || make_pair(3, 0).a);
  putintln(yes || make_pair(3, 0).a);
  putintln(make_pair(7, 0).a || no); // non-0/1 lhs short-circuits a ||
  putintln(make_pair(7, 0).a && yes); // non-0/1 lhs falls through a &&
  putintln(call_count);

  return 0;
}
