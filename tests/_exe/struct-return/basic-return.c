#include <stdio.h>

void putstr(char *s) {
  while (*s) {
    putchar(*s);
    s += 1;
  }
}

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

struct Pair global_pair;

void print_pair(struct Pair p) {
  putint(p.a);
  putchar(' ');
  putint(p.b);
  putchar('\n');
}

int main() {
  struct Pair local_pair;
  struct Pair initialized_pair = make_pair(7, 8);

  // Assign call result to a local struct
  local_pair = make_pair(11, 22);
  print_pair(local_pair);

  // Assign call result to a global struct
  global_pair = make_pair(-3, 44);
  print_pair(global_pair);

  // Local initialized directly from a call
  print_pair(initialized_pair);

  // Call result passed directly as a by-value argument
  print_pair(make_pair(5, -6));

  return 0;
}
