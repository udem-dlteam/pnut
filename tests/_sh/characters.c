// Make sure all character literals are supported, and the ones that need to be
// escaped are escaped properly. The first ones are the argument of putchar,
// which can print them directly, and the second ones have to be compiled to
// their value: the awk backend keeps the printable ones as character literals
// and gives a name to the ones that awk can't write.

#include <stdio.h>

int chars[6];

int main() {
  int c;

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

  chars[0] = 'a';
  chars[1] = '\\';
  chars[2] = '\"';
  chars[3] = '\t';
  chars[4] = '\0';
  chars[5] = '$';

  c = chars[0];
  putchar(c);
  putchar(c == 'a' ? 'y' : 'n');
  putchar('\n');

  putchar(chars[1]);
  putchar(chars[2]);
  putchar(chars[3] != 0 ? chars[3] : '?');
  putchar('\n');

  // A character used in arithmetic, and one that is zero.
  putchar('0' + 3);
  putchar('\n');
  putchar(chars[4]);
  putchar(chars[5]);
  putchar('\n');
}
