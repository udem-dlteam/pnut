#include <stdio.h>

void putstr(char *str) {
  while (*str) {
    putchar(*str);
    str++;
  }
}

void main() {
  char *str = "Hello, World!\n";
  if (!str) { // if str is not null
    putstr("null\n");
  } else {
    putstr(str);
  }
}
