#ifndef PNUT_CC
#include <stdio.h>
#else
typedef int FILE;
#endif

void main() {
  int i = 0;
  char c;
  // Test %d, %c, %x, %s, %.*s, %4s
  printf("Hello, world!\n");
  printf("%d\n", 42);
  printf("%c\n", 'a');
  printf("Hello %c-%xPO\n", 'C', 3);
  printf("Hello, %s!\n", "world");
  printf("Hello, world! %.*s\n", 5, "R2-D2 (beep-boop)");
  printf("Hello, world! <%12.5s>\n", "ROBOT");
}
