// 64-bit addition and subtraction, including carry/borrow across the 32-bit
// boundary and wraparound at the 64-bit boundary.
#include "int64_util.h"

int main() {
  long long a, b;

  a = 0xFFFFFFFFLL; b = 1LL;
  show64(a + b);                 // carry into high word: 100000000
  show64(a - b);                 // fffffffe

  a = 0x00000000FFFFFFFFLL; b = 0x00000000FFFFFFFFLL;
  show64(a + b);                 // 1fffffffe

  a = 0x0000000100000000LL; b = 1LL;
  show64(a - b);                 // borrow from high word: ffffffff

  a = 0xFFFFFFFFFFFFFFFFLL; b = 1LL;
  show64(a + b);                 // wrap to 0
  show64(a + a);                 // fffffffffffffffe

  // signed negatives
  a = -5LL; b = 3LL;
  show64(a + b);                 // fffffffffffffffe (-2)
  show64(a - b);                 // fffffffffffffff8 (-8)
  show64(0LL - a);               // 5

  // unsigned wrap
  unsigned long long u = 0ULL, v = 1ULL;
  show64u(u - v);                // ffffffffffffffff
  return 0;
}
