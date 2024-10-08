// comp_pnut_opt: -DOPTIMIZE_LONG_LINES

#include <stdio.h>

char* str1 = "WOWOWOWOWOWO\0HAHAHAHA";
// Make sure escape sequences spanning 16 characters boundary are handled correctly.
// This is important
char *str2 = "012345678901235\0 6789";

int main() {
  int i = 0;
  while (i < 20) {
    putchar(str1[i]);
    i += 1;
  }
}
