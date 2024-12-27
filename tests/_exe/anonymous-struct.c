#include <stdio.h>

void putstr(char* s) {
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

struct Point {
  int a;
  struct { // Anonymous struct
    int b;
    int c;
  };
};

int main() {
  struct Point p;
  p.a = 5;
  p.b = 6;
  p.c = 7;
  putint(p.a); putchar('\n');
  putint(p.b); putchar('\n');
  putint(p.c); putchar('\n');
  return 0;
}
