// Make sure we can read all 256 characters. This is to make sure the utilities
// we compile using pnut-sh can process all kind of files.
// This test is not essential for the bootstrap, but since we'd like to use pnut
// in other contexts, we want to make sure it can handle all characters.

#ifndef PNUT_CC
#include <stdio.h>
#else
typedef int FILE;
#endif

void main() {
  int i = 0;
  char c;
  FILE *f = fopen("tests/input-output/all-chars.txt", "r");
  while ((c = fgetc(f)) != -1) {
    putchar(c);
  }

  putchar('\n');
}
