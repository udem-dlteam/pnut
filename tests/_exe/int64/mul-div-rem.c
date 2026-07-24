// 64-bit multiplication, division and remainder, signed and unsigned.
#include "int64_util.h"

int main() {
  long long a, b;

  a = 0x100000000LL; b = 3LL;
  show64(a * b);                 // 300000000
  show64(a / b);                 // 55555555
  show64(a % b);                 // 1

  a = 0xFFFFFFFFLL; b = 0xFFFFFFFFLL;
  show64(a * b);                 // fffffffe00000001

  // signed division rounds toward zero; remainder takes dividend's sign
  a = -100LL; b = 7LL;
  show64(a / b);                 // ffffffffffffffff2 -> -14
  show64(a % b);                 // fffffffffffffffe  -> -2
  a = 100LL; b = -7LL;
  show64(a / b);                 // -14
  show64(a % b);                 // 2

  // unsigned division of a value with the high bit set
  unsigned long long u = 0xFFFFFFFFFFFFFFFFULL, v = 0x100000000ULL;
  show64u(u / v);                // ffffffff
  show64u(u % v);                // ffffffff

  // large signed product wrapping
  a = 0x0000000200000000LL; b = 0x0000000000000003LL;
  show64(a * b);                 // 600000000
  return 0;
}
