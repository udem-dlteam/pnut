// 64-bit operations mixing a long long with a narrower unsigned operand.
// C's usual arithmetic conversions widen the narrower unsigned operand to
// (signed) long long, so the operation stays signed. Regression test for the
// signedness selection in the 64-bit lowering: a narrower unsigned operand must
// not, on its own, turn a signed `long long` division/remainder/comparison into
// an unsigned one, and a right shift is signed iff its left operand is.
#include "int64_util.h"

int main() {
  long long a = -8LL;
  unsigned int b = 2;

  // unsigned int widens to signed long long: signed division and remainder
  show64(a / b);                 // ffffffff fffffffc  (-4, not unsigned 7ff...c)
  show64(a % b);                 // 0
  a = -9LL;
  show64(a % b);                 // ffffffff ffffffff  (-1, dividend's sign)

  // right shift by an unsigned count stays arithmetic (left operand is signed)
  a = -8LL;
  show64(a >> b);                // ffffffff fffffffe  (-2, sign-extended)

  // ordered comparison stays signed: -8 < 2 is true (1), not a huge unsigned
  showint(a < b);                // 1
  showint(a > b);                // 0

  // when the long long itself is unsigned, the operation is unsigned
  unsigned long long u = 0xFFFFFFFFFFFFFFF8ULL;
  show64u(u / b);                // 7fffffff fffffffc
  return 0;
}
