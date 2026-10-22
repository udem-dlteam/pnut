// Identifiers that are rejected by the "macro is already expanding" check are
// painted blue: they must never be expanded again, even when they are replayed
// later on, once the macro whose expansion produced them is gone.
//
// The paint matters for macro arguments: an argument is expanded before being
// substituted, so by the time its tokens are replayed inside the replacement
// list, the macros that were being expanded are no longer on the macro stack.
// Without the paint bit, `#define z z + 1` used as `f(z)` expands forever.

// putchar
#include <stdio.h>

int z = 4;
int w = 1;

// Self-referencing object-like macros
#define z (z + 1)
#define w w + 1

#define I(x) ((x) + 1)
#define P(x) ((x) + (x))

// Self-referencing function-like macro: `fp(1)` is the painted invocation that
// is left in the output, so `fp` must also be a real function.
int fp(int n) {
  return n + 1;
}
#define fp(x) x(1)
#define g fp

void putdigit(int n) {
  putchar('0' + n);
  putchar('\n');
}

void main() {
  putdigit(z);     // (z + 1)                = 5
  putdigit(I(z));  // ((z + 1) + 1)          = 6
  putdigit(P(w));  // ((w + 1) + (w + 1))    = 4
  putdigit(I(I(z)));// (((((z + 1)) + 1)) + 1) = 7
  putdigit(fp(g)); // fp(1)                  = 2
}
