// Mixed int / long long expressions exercise operand coercion in both
// directions and the usual arithmetic conversions.
#include "int64_util.h"

int main() {
  long long a = 0x100000000LL;
  signed   int n = 5;

  show64(a + n);                 // 100000005  (int widened)
  show64(n + a);                 // 100000005
  show64(a - 1);                 // ffffffff
  show64(a * 2);                 // 200000000
  show64(2 * a);                 // 200000000

  // result of a 64-bit op narrowed back to int
  int low = (int)(a + 0x55);     // 0x55
  showint(low);                  // 55
  int hi_nonzero = (int)(a >> 32); // 1
  showint(hi_nonzero);           // 1

  // nested mixed
  show64((a + 10) - (a - 10));   // 14

  // unsigned int widened to unsigned long long keeps zero-extension
  unsigned int big = 0x80000000;
  long long w = big;             // zero-extend, not sign-extend
  show64(w);                     // 80000000

  long long s = -1;              // small signed int sign-extends
  show64(s);                     // ffffffffffffffff

  long long d = -8LL;
  unsigned int b = 2;
  unsigned long long u = 0xFFFFFFFFFFFFFFF8ULL;
  // unsigned int widens to signed long long: signed division and remainder
  show64(d / b);                 // ffffffff fffffffc  (-4, not unsigned 7ff...c)
  show64(d % b);                 // 0
  d = -9LL;
  show64(d % b);                 // ffffffff ffffffff  (-1, dividend's sign)

  // right shift by an unsigned count stays arithmetic (left operand is signed)
  d = -8LL;
  show64(d >> b);                // ffffffff fffffffe  (-2, sign-extended)

  // ordered comparison stays signed: -8 < 2 is true (1), not a huge unsigned
  showint(d < b);                // 1
  showint(d > b);                // 0

  // when the long long itself is unsigned, the operation is unsigned
  show64u(u / b);                // 7fffffff fffffffc
  return 0;
}
