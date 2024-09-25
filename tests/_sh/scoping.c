#include <stdio.h>

int glo1 = 12;

void main() {
  printf("%d\n", glo1);
  {
    int glo1 = 13;
    printf("%d\n", glo1);
  }
  printf("%d\n", glo1);
}
