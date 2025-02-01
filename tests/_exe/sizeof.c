#include <stdio.h>

// The testing infrastructure doesn't distinguish between 32 bit and 64 bit
// tests, so we need to divide the size by 2 in the 64 bit case.

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

int main() {
  int a[10] = { 0, 1, 2, 3 };
  int b[]   = { 0, 1, 2, 3 }; // Infer size from initializer (4 elements)
  putint(sizeof(char));             putchar('\n'); // No division needed because sizeof(char) is always 1
  putint(sizeof(int) / MUL);        putchar('\n');
  putint(sizeof(int[10]) / MUL);    putchar('\n');
  putint(sizeof(int[10][2]) / MUL); putchar('\n');
  putint(sizeof(a) / MUL);          putchar('\n'); // sizeof (expr)
  putint(sizeof a / MUL);           putchar('\n'); // sizeof expr
  putint(sizeof(b) / MUL);          putchar('\n'); // sizeof (expr)
  putint(sizeof b / MUL);           putchar('\n'); // sizeof expr
  putint(sizeof((void *) a) / MUL); putchar('\n'); // sizeof (cast_expr)

  return 0;
}
