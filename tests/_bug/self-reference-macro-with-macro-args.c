// When a macro argument is expanded, tokens that are themselves macros are
// expanded. This is problematic when the macro argument contains a
// self-referencial macro, since each time the macro is passed as argument, it
// is expanded for one step, even when the macro should not be expanded because
// it was expanded previously.

// expect_failure

#include <stdio.h>

int x = 4;
int y = 5;

void putdigit(int n) {
  putchar('0' + n);
  putchar('\n');
}

void B(int a) {
  putdigit(a);
}

#define x 1 + y
#define y 1 + x
#define A(a1) B(a1)
#define B(a1) A(a1)

void main() {
  B(x);
  B(y);
}
