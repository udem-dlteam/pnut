// Macro arguments are expanded at the *call site*, before the replacement list
// of the invoked macro is scanned.  Because the name of the invoked macro is
// not hidden at the call site, a macro can appear in its own argument.
//
// This is the `CAR(CAR(x))` pattern of examples/repl.c: `#define CAR(x) RIB(x)`
// used to leave the inner `CAR(...)` unexpanded, because the argument was
// expanded inside the dynamic extent of CAR itself, where CAR is hidden.

// putchar
#include <stdio.h>

int v = 1;

// A macro whose body calls another macro, like `#define CAR(x) RIB(x)->field0`
#define RIB(z) (z + 1)
#define CAR(z) RIB(z)

#define INC(z) ((z) + 1)
#define TWICE(z) ((z) + (z))
#define ADD(a, b) ((a) + (b))
#define SWAP_ADD(a, b) ADD(b, a)
#define ID(z) (z)

// A chain of `ID` calls that are each the argument of the next one. Each level is
// expanded at its own call site and costs 6 entries of the macro stack (see
// MACRO_RECURSION_MAX), so this checks that an argument expansion does not use up
// more than the frame of the expansion itself.

void putdigit(int n) {
  putchar('0' + n);
  putchar('\n');
}

void main() {
  putdigit(CAR(v));                // RIB(v)            = 2
  putdigit(CAR(CAR(v)));           // RIB(RIB(v))       = 3
  putdigit(CAR(CAR(CAR(v))));      //                   = 4
  putdigit(CAR(CAR(CAR(CAR(v))))); //                   = 5
  putdigit(INC(INC(INC(0))));      // ((0 + 1) + 1) + 1 = 3
  putdigit(TWICE(TWICE(1)));       //                   = 4
  putdigit(ADD(INC(1), INC(2)));   // 2 + 3             = 5
  putdigit(SWAP_ADD(1, TWICE(2))); // 4 + 1             = 5
  putdigit(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(ID(7)))))))))))))))))))))))))))))))));                     // 32 levels deep     = 7
}
