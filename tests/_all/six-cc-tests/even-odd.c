#include <stdio.h>

void putstr(char *str) {
  while (*str) {
    putchar(*str);
    str += 1;
  }
}

void putint_aux(int n) {
  if (n <= -10) putint_aux(n / 10);
  putchar('0' - (n % 10));
}

void putint(int n) {
  if (n < 0) {
    putchar('-');
    putint_aux(n);
  } else {
    putint_aux(-n);
  }
}

int abs(int number);
int even(int number);
int odd(int number);

int abs(int number) {
  if(number < 0) return -number;
  return number;
}

int even(int number) {
  int a; /* Local variable so that the function is not simple */
  if(number == 0) return 1;
  return odd(abs(number)-1);
}

int odd(int number) {
  int a; /* Local variable so that the function is not simple */
  if( number == 0 ) return 0;
  return even(abs(number)-1);
}

int main() {
  int n1;
  int n2;
  n1 = even(10);
  n2 = odd(10);
  putstr("n1 = ");
  putint(n1);
  putchar('\n');
  putstr("n2 = ");
  putint(n2);
  putchar('\n');
  return 0;
}
