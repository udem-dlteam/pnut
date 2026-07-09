// Mixed int / long long expressions exercise operand coercion in both
// directions and the usual arithmetic conversions.
#include "int64_util.h"

int main() {
  long long a = 0x100000000LL;
  int n = 5;

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
  return 0;
}
