#include <stdio.h>
void main() {
  int i = 5;
  switch (i) {
    case 0: {
      putchar('0');
      break;
    }
    case 1: {
      putchar('1');
      break;
    }
    case 5: {
      putchar('>');
      {
        putchar('4');
        break;
      }
      putchar('=');
      putchar('5');
      break;
    }
    default: {
      putchar('D');
      break;
    }
  }
}
