// Make sure we can read unicode characters. This is to make sure the utilities
// we compile using pnut-sh can process all kind of files.
// This test is not essential for the bootstrap but being able to properly read
// non-ascii characters makes pnut more general.
//
// Yash's IO is a little bit weird and it's not a very popular shell, disabling the test for now.
// expect_failure_for: yash
// expect_failure_for: osh

#ifndef PNUT_CC
#include <stdio.h>
#else
typedef int FILE;
#endif

void putstr(const char *s) {
  while (*s) {
    putchar(*s);
    s++;
  }
}

void main() {
  int i = 0;
  char c;
  FILE *f = fopen("tests/_all/input-output/unicode.txt", "r");
  putstr("printf? 💣\n");
  while ((c = fgetc(f)) != -1) {
    putchar(c);
  }
}
