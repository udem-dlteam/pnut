// tests for __FILE__, __LINE__, __DATE__, __TIME__, __TIMESTAMP__ built-in macros
// comp_pnut_opt: -USAFE_MODE
#include <stdio.h>

#ifndef __FILE__
#error "__FILE__ is not defined"
#endif
#ifndef __LINE__
#error "__LINE__ is not defined"
#endif
#ifndef __DATE__
#error "__DATE__ is not defined"
#endif
#ifndef __TIME__
#error "__TIME__ is not defined"
#endif
#ifndef __TIMESTAMP__
#error "__TIMESTAMP__ is not defined"
#endif

void putint(int n) {
  if (n < 0) {
    putchar('-');
    putint(-n);
  } else if (n > 9) {
    putint(n / 10);
    putchar('0' + n % 10);
  } else {
    putchar('0' + n);
  }
}

void putstr(char *str) {
  while (*str) {
    putchar(*str);
    str += 1;
  }
}

int main() {
  putstr(__FILE__);      putchar('\n');
  putint(__LINE__);      putchar('\n');
  putstr(__DATE__);      putchar('\n');
  putstr(__TIME__);      putchar('\n');
  putstr(__TIMESTAMP__); putchar('\n');
  return 0;
}
