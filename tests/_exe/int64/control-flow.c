// Truthiness, control flow, logical operators on 64-bit values.
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
  for (i = 0; i < 5; ++i) s += i;
  show64(s);                     // sum of 0..4

  long long p = 1;
  int k = 10;
  while (k > 0) { p *= 2; --k; }  // 2^10 = 400
  show64(p);

  // Ternary
  long long x = 0x1234567890abcdefLL;
  long long y = 0xdeadbeefdeadbeefLL;
  show64(long_long_identity(x > y) ? x : y);       // == show64(x)
  show64(long_long_identity(x < y) ? x : y);       // == show64(y)
  show64((long_long_identity(x < y)) ? (x += 5) : (y += 10)); // show64(y += 10)
  show64(x);
  show64(y);

  return 0;
}
