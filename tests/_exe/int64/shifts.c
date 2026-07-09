// 64-bit shifts at and across the 32-bit boundary, logical and arithmetic.
#include "int64_util.h"

int main() {
  long long a = 1LL;
  show64(a << 0);                // 1
  show64(a << 31);               // 80000000
  show64(a << 32);               // 100000000
  show64(a << 63);               // 8000000000000000

  a = 0x123456789ABCDEFLL;
  show64(a << 4);                // 123456789abcdef0
  show64(a >> 4);                // 0123456789abcde

  // arithmetic right shift sign-extends
  a = 0x8000000000000000LL;
  show64(a >> 4);                // f800000000000000
  show64(a >> 63);               // ffffffffffffffff
  show64(a >> 32);               // ffffffff80000000

  // logical right shift on unsigned zero-fills
  unsigned long long u = 0x8000000000000000ULL;
  show64u(u >> 4);               // 0800000000000000
  show64u(u >> 63);              // 1
  show64u(u >> 32);              // 80000000
  return 0;
}
