// 64-bit literals: suffixes, hex/decimal, values that don't fit in 32 bits,
// and small literals widened by coercion.
#include "int64_util.h"

long long g1 = 0x123456789ABCDEFLL;   // global initialized from a 64-bit literal
long long g2 = 1000000000000LL;       // decimal that doesn't fit in 32 bits
long long g3 = 42;                    // small literal widened to long long
unsigned long long g4 = 0xFFFFFFFFFFFFFFFFULL;

int main() {
  show64(g1);                    // 0123456789abcdef
  show64(g2);                    // e8d4a51000
  show64(g3);                    // 2a
  show64u(g4);                   // ffffffffffffffff

  long long a = 0x100000000LL;
  show64(a);                     // 100000000
  show64(0xDEADBEEFCAFEBABELL);  // deadbeefcafebabe
  show64(5LL);                   // 5
  show64(-7LL);                  // fffffffffffffff9
  show64u(18446744073709551615ULL); // ffffffffffffffff
  return 0;
}
