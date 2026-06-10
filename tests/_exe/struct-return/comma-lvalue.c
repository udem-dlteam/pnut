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

int counter = 0;

int bump() {
  counter = counter + 1;
  return counter;
}

int main() {
  struct Pair p;
  int x = 0;

  // Struct assignment whose rhs is a comma expression
  p = (x = 10, make_pair(1, 2));
  putintln(x);
  putintln(p.a);
  putintln(p.b);

  // Initialization from a comma expression
  struct Pair q = (x = 20, make_pair(3, 4));
  putintln(x);
  putintln(q.a + q.b);

  // Member access on a comma expression
  putintln((x = 30, make_pair(5, 6)).a);
  putintln(x);

  // Comma lhs is itself a struct-returning call, burying temporaries
  putintln((make_pair(7, 8), make_pair(9, 10)).b);

  // Nested commas with side effects
  p = (bump(), bump(), make_pair(11, 12));
  putintln(counter);
  putintln(p.a + p.b);

  return 0;
}
