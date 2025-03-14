#include <stdio.h>

void putstr(char* s) {
  while (*s) {
    putchar(*s);
    s++;
  }
}

void main() {
  int a = 0x12345678;

  // Tests that casts properly truncate the value
  if (a == (char) a) {
    putstr("!!! a == (char) a\n");
  } else {
    putstr("a != (char) a\n");
  }

  // Again
  if (a == (short) a) {
    putstr("!!! a == (short) a\n");
  } else {
    putstr("a != (short) a\n");
  }

  // Again, but this time the cast is to int so it should be equal
  if (a == (int) a) {
    putstr("a == (int) a\n");
  } else {
    putstr("!!! a != (int) a\n");
  }

  // Again, but with a cast to a wider type
  if (a == (long) a) {
    putstr("a == (long) a\n");
  } else {
    putstr("!!! a != (long) a\n");
  }
}
