#include <stdio.h>

static int n_calls = 0; // Global static variable

int f() {
  int i = 0;              // Some local varaible
  static int n_calls = 0; // Local static variable shadowing the global one
  int j = 10;
  return n_calls++ + i + j;
}

void putint(int n) {
  if (n < 0) {
    putchar('-');
    n = -n;
  }
  if (n >= 10) {
    putint(n / 10);
  }
  putchar('0' + n % 10);
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
  return 0;
}
