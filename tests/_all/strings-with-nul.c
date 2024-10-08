#include <stdio.h>

char* str = "WOWOWOWOWOWO\0HAHAHAHA";

int main() {
  int i = 0;
  while (i < 20) {
    putchar(str[i]);
    i += 1;
  }
}
