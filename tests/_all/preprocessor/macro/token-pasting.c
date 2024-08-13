// tests for macro token pasting operator

// putchar
#include <stdio.h>

// Token pasting

int number1234 = 5678;

#define VAR number
#define CONST_PASTED 1234 ## 5678
#define VAR_PASTED number ## 1234

#define PASTE(A, B) A ## B

#define PASTED2(X) 1234 ## X
#define PASTED3(X) X ## 1234

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
  putchar('\n');
}

void main() {
  putint(CONST_PASTED);         // 12345678
  putint(VAR_PASTED);           // number1234
  putint(PASTE(number, 1234));  // number1234
  putint(PASTED2(5678 + 1));    // 12345678 + 1
  putint(PASTED3(5678 + 1));    // 5678 + 11234
}
