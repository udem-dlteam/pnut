#include <stdio.h>

void reverse_str(char* str) {
  char* end = str;
  char tmp;
  int len, i = 0;
  while (*(end++)) ++len; // Compute length of string
  --len;

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
    printf("argv[%d] = %s\n", i, argv[i]);
  }
}
