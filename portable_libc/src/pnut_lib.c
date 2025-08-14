#include <pnut_lib.h>
#include <stdlib.h>
#include <stdio.h>

void pnut_debug(const char *str) {
  while (*str) {
    putchar(*str);
    str++;
  }
}

void pnut_abort(const char *str) {
  while (*str) {
    putchar(*str);
    str++;
  }
  exit(1);
}
