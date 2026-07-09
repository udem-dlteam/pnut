// Exercises ternary operator with 64-bit values and temporaries.
#include "int64_util.h"

void take(long long x) { show64(x); }

int main() {
  long long a, b, c;
  int cond = 1;

  c = a = b = 0x1122334455667788LL;   // chained 64-bit assignment
  show64(a); show64(b); show64(c);

  take(a = 0x2aLL);                   // assignment used by value as argument
  show64(a);

  show64(cond ? a : 0);                     // ternary, narrow false arm, take true
  show64(cond ? 7 : a);                     // ternary, narrow true arm, take true
  show64(cond ? long_long_identity(7) : a); // ternary, wide true arm, take true
  cond = 0;
  show64(cond ? long_long_identity(a) : 5); // ternary, narrow false arm, take false
  take(cond ? 100 : a);                     // ternary result passed by value
  return 0;
}
