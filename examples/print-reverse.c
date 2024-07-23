/*
 * print-reverse.c: Print command-line arguments in reverse order
 *
 * Usage: ./print-reverse.sh
 */

#include <stdio.h>

void reverse_str(char* str) {
  char* end = str;
  char tmp;
  int len, i = 0;
  while (*(end++)); // Compute length of string
  len = end - str - 1;

  for (; i < len / 2; ++i) {
    tmp = str[i];
    str[i] = str[len - 1 - i];
    str[len - 1 - i] = tmp;
  }
}

void main(int argc, char **argv) {
  int i;
  for (i = 1; i < argc; ++i) {
    reverse_str(argv[i]);
    printf("%s\n", i, argv[i]);
  }
}
