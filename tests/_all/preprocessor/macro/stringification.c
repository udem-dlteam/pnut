// tests for macro stringification operator

// putchar
#include <stdio.h>

#define STR(x) #x

void putstring(char * s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
  putchar('\n');
}

void main() {
  putstring(STR(abc));
  putstring(STR(5)); // This should be "5", but integers are not supported so it returns NOT_SUPPORTED
}
