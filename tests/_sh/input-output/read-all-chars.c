// Make sure we can read all 256 characters. This is to make sure the utilities
// we compile using pnut-sh can process all kind of files.
// This test is not essential for the bootstrap, but since we'd like to use pnut
// in other contexts, we want to make sure it can handle all characters.
// For bash 2.05a, the character 1 is not read properly. Assuming all version 2.0* of bash are affected.
// expect_failure_for: bash-2.0.*
// Yash's IO is a little bit weird and it's not a very popular shell, disabling the test for now.
// expect_failure_for: yash
// expect_failure_for: osh

#ifndef PNUT_CC
#include <stdio.h>
#else
typedef int FILE;
#endif

void main() {
  int i = 0;
  char c;
  FILE *f = fopen("tests/_sh/input-output/all-chars.txt", "r");
  while ((c = fgetc(f)) != -1) {
    putchar(c);
  }
}
