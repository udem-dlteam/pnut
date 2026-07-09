// Shared helpers for the int64 tests. Results are printed as hex halves (high
// then low 32-bit word) so the same sources and golden files validate on both
// the i386 backend (where long long is lowered to arith64.c calls) and natively
// on x86_64 (where long long is a 64-bit scalar).
#include <stdio.h>

void puthex32_(unsigned int n) {
  int sh = 28;
  unsigned int d;
  while (sh >= 0) {
    d = (n >> sh) & 0xf;
    putchar(d < 10 ? ('0' + d) : ('a' + d - 10));
    sh -= 4;
  }
}

void show64(long long x) {
  unsigned int *p = (unsigned int*)&x;
  puthex32_(p[1]);
  puthex32_(p[0]);
  putchar('\n');
}

void show64u(unsigned long long x) {
  unsigned int *p = (unsigned int*)&x;
  puthex32_(p[1]);
  puthex32_(p[0]);
  putchar('\n');
}

void showint(int v) {
  puthex32_((unsigned int)v);
  putchar('\n');
}
