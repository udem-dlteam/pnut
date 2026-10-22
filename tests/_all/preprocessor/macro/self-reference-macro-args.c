// When a macro is passed as an argument to another macro, the expansion of the argument must
// not be undone or repeated: the tokens that were left unexpanded because their macro was
// already being expanded stay unexpanded, no matter how many macros they go through.

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
