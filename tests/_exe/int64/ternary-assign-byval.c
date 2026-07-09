// Exercises codegen paths added alongside the int64 literal-recursion fix:
// chained 64-bit assignment (codegen_lvalue '=' case), a 64-bit assignment used
// by value as an argument, and ternaries whose common type is long long but one
// arm is a narrower scalar (widened to 64-bit before codegen).
#include "int64_util.h"

void take(long long x) { show64(x); }

int main() {
  long long a, b, c;
  int cond = 1;

  c = a = b = 0x1122334455667788LL;   // chained 64-bit assignment
  show64(a); show64(b); show64(c);

  take(a = 0x2aLL);                   // assignment used by value as argument
  show64(a);

  show64(cond ? a : 0);              // ternary, narrow false arm, take true
  show64(cond ? 7 : a);              // ternary, narrow true arm, take true
  cond = 0;
  show64(cond ? a : 5);             // ternary, narrow false arm, take false
  take(cond ? 100 : a);            // ternary result passed by value
  return 0;
}
