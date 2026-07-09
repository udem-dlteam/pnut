// Truthiness, control flow, logical operators, compound assignment and
// pre-increment/decrement on long long values.
#include "int64_util.h"

int main() {
  long long a = 5, z = 0;

  // truthiness
  showint(!z);                   // 1
  showint(!a);                   // 0
  if (a) showint(0x11); else showint(0x22);  // 11
  if (z) showint(0x11); else showint(0x22);  // 22
  showint(a && 1);               // 1
  showint(z && 1);               // 0
  showint(z || a);               // 1
  showint(z || 0);               // 0
  showint(a ? 0x33 : 0x44);      // 33  (64-bit condition)
  showint(z ? 0x33 : 0x44);      // 44

  // loops with long long accumulator and counter
  long long s = 0;
  long long i;
  for (i = 0; i < 5; ++i) s += i; // 0+1+2+3+4 = a
  show64(s);                     // a

  long long p = 1;
  int k = 10;
  while (k > 0) { p *= 2; --k; }  // 2^10 = 400
  show64(p);

  // compound assignment operators
  long long b = 0xF0LL;
  b += 0x0FLL; show64(b);        // ff
  b -= 0x10LL; show64(b);        // ef
  b *= 0x10LL; show64(b);        // ef0
  b /= 0x2LL;  show64(b);        // 778
  b %= 0x100LL; show64(b);       // 78
  b <<= 8;     show64(b);        // 7800
  b >>= 4;     show64(b);        // 780
  b &= 0x7F0LL; show64(b);       // 780
  b |= 0x00FLL; show64(b);       // 78f
  b ^= 0xFFFLL; show64(b);       // 870

  // pre inc/dec
  long long c = 0xFFFFFFFFLL;
  ++c; show64(c);                // 100000000
  --c; show64(c);                // ffffffff
  return 0;
}
