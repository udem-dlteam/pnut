#include <stdio.h>

typedef int bool;
#define true 1
#define false 0

void putstr(char* s) {
  while (*s) putchar(*s++);
}

// Runtime configuration
#define RT_FREE_UNSETS_VARS
#define RT_INIT_GLOBALS
#define RT_COMPACT_not

#define INCLUDE_ALL_RUNTIME

#include "sh-runtime.c"

void main() {
  produce_runtime();
}
