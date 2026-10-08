// Tests the initializer lists of global arrays. The array size is optional when
// there is an initializer list, and the elements that the list doesn't cover are
// set to 0.

#include <stdio.h>

int unsized[] = { 7, 8, 9 };
int padded[6] = { 1, 2 };
int with_chars[3] = { 'a', 'z', '\n' };
char greeting[8] = "hi";
char *words[] = { "one", "two", "three" };

void putstr(char *str) {
  while (*str) {
    putchar(*str);
    str = str + 1;
  }
}

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

void putint_line(int n) {
  putint(n);
  putchar('\n');
}

void putstr_line(char *str) {
  putstr(str);
  putchar('\n');
}

void main() {
  int i;

  for (i = 0; i < 3; i++) putint_line(unsized[i]);
  // The 4 elements that the list doesn't cover are 0.
  for (i = 0; i < 6; i++) putint_line(padded[i]);
  for (i = 0; i < 3; i++) putint_line(with_chars[i]);

  // "hi" is copied into the array, with a null terminator followed by 0s.
  for (i = 0; i < 8; i++) putint_line(greeting[i]);
  putstr_line(greeting);

  for (i = 0; i < 3; i++) putstr_line(words[i]);
}
