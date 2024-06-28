#include <stdio.h>

void print_reverse(char* str) {
  if (*str) {
    print_reverse(str + 1);
    putchar(*str);
  }
}

void main(int argc, char **argv) {
  int i;
  for (i = 1; i < argc; ++i) {
    print_reverse(argv[i]);
    putchar('\n');
  }
}
