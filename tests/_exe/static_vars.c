#include <stdio.h>

void putint(int n) {
  if (n < 0) {
    putchar('-');
    n = -n;
  }
  if (n > 9) {
    putint(n / 10);
  }
  putchar('0' + n % 10);
}

static int n_calls = 0; // Global static variable

int f() {
  int i = 0;              // Some local varaible
  static int n_calls = 0; // Local static variable shadowing the global one
  int j = 10;
  return n_calls++ + i + j;
}

struct S {
  int a[10];
};

void g() {
  static struct S s_g = { { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 } };

  int i = 0;
  while (i < 10) {
    putchar('\''); putint(s_g.a[i]); putchar('\''); putchar(' ');
    i++;
  }
}

void h() {
  static struct S s_h = { { 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 } };

  int i = 0;
  while (i < 10) {
    putchar('\''); putint(s_h.a[i]); putchar('\''); putchar(' ');
    i++;
  }
  putchar('\n');
}

int main() {
  putint(n_calls); putchar('\n');
  putint(f());     putchar('\n');
  putint(f());     putchar('\n'); // Check that the local static variable is incremented
  n_calls = 5;                    // Modifying the global static variable, not the local one
  putint(n_calls); putchar('\n');
  putint(f());     putchar('\n'); // Check that the local static variable wasn't modified by `n_calls = 5`
  putint(n_calls); putchar('\n');
  putint(f());     putchar('\n');

  g();
  h();
  g();
  h();
  return 0;
}
