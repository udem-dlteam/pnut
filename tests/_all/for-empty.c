#include <stdio.h>

int main() {
  int i = 0;
  for (;;) {
    if (i == 10) {
      return 0;
    }
    putchar('0' + i);
    i += 1;
  }
}
