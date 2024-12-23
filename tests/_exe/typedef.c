#include <stdio.h>
#include <stdlib.h>

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
  int b;
};

typedef struct Point3 {
  int x;
  int y;
  int z;
} Point; // Voluntarily redefine Point

void main() {
  struct Point p;
  p.a = 5;
  p.b = 6;
  putstr("Point: "); putint(p.a); putstr(" "); putint(p.b); putchar('\n');
  Point p3;
  p3.x = 7;
  p3.y = 8;
  p3.z = 9;
  putstr("Point: "); putint(p3.x); putstr(" "); putint(p3.y); putstr(" "); putint(p3.z); putchar('\n');
}
