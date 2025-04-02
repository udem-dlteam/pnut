#include <stdio.h>

#if defined (PNUT_I386)
#define MUL 1
#elif defined (PNUT_X86_64)
#define MUL 2
#else
#define MUL 2
#endif

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

void f(int a[10]) {
  int b[10];
  putint(sizeof(a) / MUL); putchar('\n'); // Should be a pointer
  putint(sizeof(b)); putchar('\n'); // Should be 10 * sizeof(int)
  int i = 0;
  while (i < 10) {
    putchar('\''); putint(a[i]); putchar('\''); putchar(' ');
    i++;
  }
}

int main() {
  int a[10] = { 0, 1, 2, 3 };
  f(a); putchar('\n');
  return 0;
}
