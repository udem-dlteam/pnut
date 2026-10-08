// Tests that the string literals that contain escape sequences are copied to the
// memory of the generated programs as they are. The shell and awk backends
// define the strings with `defstr`, which used to decode the escape sequences a
// second time on awk, dropping the backslashes of "a\\b" for example.

#include <stdio.h>

char *backslashed = "a\\b";
char *with_tab = "x\ty";
char *arr_lit = "q\"q";

void putstring(char * s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
  putchar('\n');
}

int str_len(char * s) {
  int n = 0;
  while (*s) {
    ++n;
    s = s + 1;
  }
  return n;
}

void putint_line(int n) {
  putchar('0' + n);
  putchar('\n');
}

void main() {
  char *buf = "c:\\tmp\\x";
  char *quote = "it's";

  putstring("a\\b");                    // a\b
  putstring("q\"q");                    // q"q
  putstring("it's");                    // it's
  putstring("dollar:$`?");              // dollar:$`?
  putstring(backslashed);               // a\b
  putstring(with_tab);                  // x<tab>y
  putstring(quote);                     // it's
  putstring(buf);                       // c:\tmp\x
  putstring(arr_lit);                   // q"q

  // An escape sequence counts as a single character, which is what the length
  // of the string reflects.
  putint_line(str_len("a\\b"));         // 3
  putint_line(str_len("\n"));           // 1
  putint_line(str_len("a\\b\"'`$"));    // 7
  putint_line(str_len(buf));            // 8

  // The characters of a string with escapes can be compared one by one
  if (backslashed[1] == '\\') { putstring("bs"); } else { putstring("no bs"); }
  if (arr_lit[1] == '"') { putstring("dq"); } else { putstring("no dq"); }
}
