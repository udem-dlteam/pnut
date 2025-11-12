// comp_pnut_opt: -DSH_OPTIMIZE_LONG_LINES=

#include <stdio.h>

char* str1 = "WOWOWOWOWOWO\0HAHAHAHA";
// Make sure escape sequences spanning 16 characters boundary are handled correctly.
char *str2 = "012345678901235\0 6789";

// 32 NUL characters, followed by 4 non-NUL characters
char *str3 = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0test";

// String concatenation and stringizing works with NUL characters
char *str4 = "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "\0" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z";

void putstrl(char* str, int len) {
  int i = 0;
  while (i < len) {
    if (str[i] == '\0') {
      putchar('_');
    } else {
      putchar(str[i]);
    }
    i += 1;
  }
  putchar('\n');
}

int main() {
  putstrl(str1, 20);
  putstrl(str2, 20);
  putstrl(str3 + 32, 4);
  putstrl(str4, 27);
  return 0;
}
