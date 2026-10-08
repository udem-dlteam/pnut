// Awk uses some names for its built-in functions, special variables, and for the
// functions of the pnut-awk runtime, and refuses them as variable names. Because
// the local variables of pnut-awk keep the name they have in the C source, those
// names get a `_` added. This program uses a few of them as parameters and as
// locals, which every backend has to compile the same way.

#include <stdio.h>

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

int sum_of(int length, int index, int match, int split, int sub) {
  int defstr;
  int substr;

  defstr = length + index;
  substr = match + split + sub;
  return defstr + substr;
}

void putstr_line(char *printf) {
  while (*printf) {
    putchar(*printf);
    printf = printf + 1;
  }
  putchar('\n');
}

void main() {
  int toupper;
  int close;
  int length;

  putint(sum_of(1, 2, 3, 4, 5));
  putstr_line("reserved names");

  for (toupper = 0; toupper < 3; toupper = toupper + 1) {
    close = toupper * 10;
    putint(close);
  }

  // A local and a parameter with the same name in different functions.
  length = 42;
  putint(length);
  putint(sum_of(length, 1, 2, 3, 4));
}
