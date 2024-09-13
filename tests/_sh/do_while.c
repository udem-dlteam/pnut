#include <stdio.h>

int factorial(int num) {
  int ret = 1;
  
  do {
    ret *= num;
    num--;
  } while(num > 0);

  return ret;
}

void main() {
  printf("Factorial of 5: %d\n", factorial(5));
  printf("Factorial of 10: %d\n", factorial(10));
}
