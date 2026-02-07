#include <stdio.h>
#include <stdlib.h>

void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void putnumber(int n) {
  int acc = 0;
  int i = 0;
  int *digits = malloc(10 * sizeof(int)); // Dynamically allocate memory for digits

  if (digits == 0) {
    putstring("Memory allocation failed\n");
    return;
  }

  if (n == 0) {
    putchar(48);
    free(digits); // Free allocated memory
    return;
  }

  while (n > 0) {
    digits[i] = n % 10;
    n = n / 10;
    i++;
  }
  i--;
  while (i >= 0) {
    putchar(digits[i] + 48);
    i--;
  }

  free(digits); // Free allocated memory
}

int main() {
  int a;
  int b;
  int x;
  int y;
  int z;

  x = 3;
  y = x++;
  x = 3;
  z = ++x;

  a = 0;
  b = 0;
  while (a < 10) {
    b += a++;
  }
  //printf("a: %d, b: %d, x: %d, y: %d, z: %d\n", a, b, x, y, z);
  putstring("a: ");
  putnumber(a);
  putchar(',');
  putchar(' ');
  putstring("b: ");
  putnumber(b);
  putchar(',');
  putchar(' ');
  putstring("x: ");
  putnumber(x);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putnumber(y);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putnumber(z);
  putchar(10);
  return 0;
}
