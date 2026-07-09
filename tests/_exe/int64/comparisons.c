// 64-bit comparisons, signed and unsigned, including sign-boundary edge cases.
#include "int64_util.h"

void cmp_signed(long long a, long long b) {
  showint(a < b); showint(a > b); showint(a <= b);
  showint(a >= b); showint(a == b); showint(a != b);
}
void cmp_unsigned(unsigned long long a, unsigned long long b) {
  showint(a < b); showint(a > b); showint(a <= b);
  showint(a >= b); showint(a == b); showint(a != b);
}

int main() {
  cmp_signed(1LL, 2LL);
  cmp_signed(2LL, 2LL);
  cmp_signed(-1LL, 1LL);                 // signed: -1 < 1
  cmp_signed(0x7FFFFFFFFFFFFFFFLL, 0x8000000000000000LL); // MAX > MIN
  cmp_signed(-5LL, -3LL);

  // The same bit patterns compared as unsigned flip ordering.
  cmp_unsigned(0xFFFFFFFFFFFFFFFFULL, 1ULL);              // huge > 1
  cmp_unsigned(0x8000000000000000ULL, 0x7FFFFFFFFFFFFFFFULL); // high-bit set is larger
  cmp_unsigned(0x100000000ULL, 0xFFFFFFFFULL);           // across boundary
  return 0;
}
