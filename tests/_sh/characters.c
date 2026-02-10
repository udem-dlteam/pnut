// Make sure all character literals are supported, and the ones that need to be
// escaped are escaped properly.

#include <stdio.h>

int main() {
  putchar('a');
  putchar('1');
  putchar(' ');
  putchar('\n');
  putchar('\t');
  putchar('\\');
  putchar('\'');
  putchar('\"');
  putchar('$');
  putchar('`');
  putchar('?');
  putchar('\0');
}