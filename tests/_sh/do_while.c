#include <stdio.h>

int bad_factorial(int num) {
  int ret = 1;
  
  do {
    ret *= num;
    num--;
  } while(num > 0);

  return ret;
}

void main() {
  printf("Factorial of 5: %d\n", bad_factorial(5));
  printf("Factorial of 10: %d\n", bad_factorial(10));
}
