#include <stdio.h>

int a = 1;
int b = 2;
int c = 3;

int foo(int a) {
  return a;
}

void show_state() {
  putchar('0' + a);
  putchar('0' + b);
  putchar('0' + c);
  putchar('\n');
}

int main() {
  show_state();
  if (foo((a,b))) {
    putchar('T'); putchar('\n');
  }
  if (foo((a++, b++, ++c)) == 4) {
    putchar('T'); putchar('\n');
  }
  show_state();

  return 0;
}
