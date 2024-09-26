// args: abc def hij
#include <stdio.h>

void putstr(char *str) {
  while (*str) {
    putchar(*str);
    str += 1;
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

int arr[1000000];
// Making sure non-word aligned globals are handled correctly. There was a bug
// once where characters were given 1 byte of space, which caused the next
// globals to be misaligned
char c = 0;

int main(int argc, char **argv) {
  int i;
  putint(argc);
  putstr("\n");
  for (i = 1; i < argc; ++i) {
    putstr(argv[i]);
    putstr("\n");
  }
  return 0;
}
