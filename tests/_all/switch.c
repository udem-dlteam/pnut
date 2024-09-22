#include <stdio.h>
int main() {
  int i = 0;
  switch (i) {
    case 0: {
      putchar('H');
      break;
    }
    case 1: {
      putchar('B');
      break;
    }
    default: {
      printf("Bad too!\n");
      break;
    }
  }
  return 0;
}
