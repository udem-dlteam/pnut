#include <stdio.h>

#define TRUE 1
#define FALSE 0

void main() {

  /* Tests basic boolean equality */

  int a = TRUE;
  if (a == TRUE) {
    putchar('t');
  } else {
    putchar('f');
  }
}
