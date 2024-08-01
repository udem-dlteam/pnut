// Make sure we can emit all 256 characters. This is to make sure all shells
// can be used to bootstrap the pnut-exe compiler, which requires emitting all
// kind of characters.

#ifndef PNUT_CC
#include <stdio.h>
#endif

void main() {
  int i = 0;
  for (; i < 256; i++) {
    putchar(i);
  }
  putchar('\n');
}
