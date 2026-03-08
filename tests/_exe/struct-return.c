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
  int lo;
  int hi;
};

struct Tiny {
  char value;
};

struct Triple {
  int a;
  int b;
  int c;
};

struct Big {
  int a;
  int b;
  int c;
  int d;
  int e;
};

struct WithArray {
  char tag;
  int data[3];
};

struct Pair make_pair(int lo, int hi) {
  struct Pair pair;
  pair.lo = lo;
  pair.hi = hi;
  return pair;
}

struct Pair forward_pair(int lo, int hi) {
  return make_pair(lo, hi);
}

struct Pair choose_pair(int which, struct Pair left, struct Pair right) {
  if (which) {
    return left;
  }
  return right;
}

struct Pair call_pair(struct Pair (*fun)(int, int), int lo, int hi) {
  return fun(lo, hi);
}

struct Tiny make_tiny(char value) {
  struct Tiny tiny;
  tiny.value = value;
  return tiny;
}

struct Triple make_triple(int a, int b, int c) {
  struct Triple triple;
  triple.a = a;
  triple.b = b;
  triple.c = c;
  return triple;
}

struct Big make_big(int a, int b, int c, int d, int e) {
  struct Big big;
  big.a = a;
  big.b = b;
  big.c = c;
  big.d = d;
  big.e = e;
  return big;
}

struct WithArray make_with_array(int x, int y, int z, char tag) {
  struct WithArray value;
  value.tag = tag;
  value.data[0] = x;
  value.data[1] = y;
  value.data[2] = z;
  return value;
}

void print_pair(char *label, struct Pair pair) {
  putstr(label);
  putstr(": ");
  putint(pair.lo);
  putstr(" ");
  putint(pair.hi);
  putchar('\n');
}

int main() {
  struct Pair a;
  struct Pair b;
  struct Pair c;
  struct Pair d = make_pair(7, 8);
  struct Pair e;
  struct Pair f;
  struct Pair (*fun)(int, int) = make_pair;
  struct Tiny tiny;
  struct Triple triple;
  struct Big big;
  struct WithArray with_array;

  // Function call as a statement, ignoring the return value
  // make_pair(1, 2);

  putstr("# direct return\n");
  a = make_pair(1, 2);
  print_pair("a", a);

  putstr("# nested return\n");
  b = forward_pair(3, 4);
  print_pair("b", b);

  putstr("# branch return\n");
  c = choose_pair(0, make_pair(9, 10), make_pair(5, 6));
  print_pair("c", c);

  putstr("# init from return\n");
  print_pair("d", d);

  putstr("# indirect return\n");
  e = fun(13, 14);
  print_pair("e", e);

  putstr("# indirect nested return\n");
  f = call_pair(fun, 15, 16);
  print_pair("f", f);

  putstr("# field from temporary\n");
  putint(make_pair(11, 22).hi);
  putchar('\n');

  putstr("# tiny return\n");
  tiny = make_tiny('A');
  putint(tiny.value);
  putchar('\n');

  putstr("# non-power-of-2 return\n");
  triple = make_triple(21, 22, 23);
  putint(triple.a);
  putstr(" ");
  putint(triple.b);
  putstr(" ");
  putint(triple.c);
  putchar('\n');

  putstr("# large return\n");
  big = make_big(31, 32, 33, 34, 35);
  putint(big.a);
  putstr(" ");
  putint(big.b);
  putstr(" ");
  putint(big.c);
  putstr(" ");
  putint(big.d);
  putstr(" ");
  putint(big.e);
  putchar('\n');

  putstr("# array field return\n");
  with_array = make_with_array(41, 42, 43, 'Z');
  putint(with_array.tag);
  putstr(" ");
  putint(with_array.data[0]);
  putstr(" ");
  putint(with_array.data[1]);
  putstr(" ");
  putint(with_array.data[2]);
  putchar('\n');

  return 0;
}
