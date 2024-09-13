#include <stdio.h>

void main() {
  int n = 0;
  do {
    printf("Enter a non-zero single-digit number:\r\n");
    n = getchar();
    while ('\n' != getchar())
      ;
  } while( n == '0' || !(n >= '0' && n <= '9'));

  printf("You entered %c: bye bye!\r\n", n);
}
