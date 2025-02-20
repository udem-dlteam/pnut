#include <stdio.h>

int test(void); // Forward declaration

int test(void) {
  putchar('T'); putchar('\n');
  return 0;
}

int main(void) {
  return test();
}
