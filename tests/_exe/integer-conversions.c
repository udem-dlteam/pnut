// Test for integer conversions.
// Converting between signed and unsigned types of the same width keeps the bit
// pattern. On backends whose registers are wider than int (x86_64), this means
// a register holding an int re-interpreted as unsigned (or vice versa) must be
// re-extended before the operations that read the high register bits: / % >>
// and the ordered comparisons.
// Note that the divisor 7 is chosen because 2^32 and 2^64 have different
// residues mod 7, so 64-bit register arithmetic cannot accidentally give the
// right 32-bit answer.
#include <stdio.h>

void putint(int n) {
  if (n < 0) {
    putchar('-');
    n = -n;
  }
  if (n >= 10) {
    putint(n / 10);
  }
  putchar('0' + n % 10);
}

unsigned int retneg() {
  return -1; // converted to unsigned at return
}

int main() {
  int si;
  unsigned int ui;
  char c;

  // signed -> unsigned cast: -16 becomes 4294967280 (0xfffffff0)
  si = -16;
  putint(((unsigned int)si) % 7);  putchar('\n'); // 2  (4294967280 % 7)
  putint(((unsigned int)si) / 7);  putchar('\n'); // 613566754
  putint(((unsigned int)si) >> 4); putchar('\n'); // 268435455 (0x0fffffff)

  // unsigned -> signed cast: 4294967280 becomes -16
  ui = -16; // stores 0xfffffff0
  putint((int)ui < 0);  putchar('\n'); // 1
  putint((int)ui / 7);  putchar('\n'); // -2
  putint((int)ui % 7);  putchar('\n'); // -2
  putint((int)ui >> 4); putchar('\n'); // -1

  // The same conversions happen implicitly through the usual arithmetic
  // conversions: the int operand is converted to unsigned int
  si = -16;
  ui = 7;
  putint(si % ui); putchar('\n'); // 2  (4294967280 % 7)
  putint((si + ui) % 7); putchar('\n'); // 2  (4294967287 % 7, i.e. -9 mod 2^32)
  ui = -2; // 4294967294
  putint(si < ui); putchar('\n'); // 1  (4294967280 < 4294967294)

  // Equality also reads the high register bits
  si = -16;
  ui = -16;
  putint(si == ui); putchar('\n'); // 1
  putint(si != ui); putchar('\n'); // 0

  // ... and so do truthiness tests: (unsigned)si + 16 wraps to 0 mod 2^32
  if ((unsigned int)si + 16) { putint(1); } else { putint(0); } putchar('\n'); // 0
  putint(((unsigned int)si + 16) ? 1 : 0); putchar('\n'); // 0
  putint(!((unsigned int)si + 16)); putchar('\n'); // 1
  putint(((unsigned int)si + 16) && 1); putchar('\n'); // 0

  // Unsigned arithmetic wraps modulo 2^32 (C99 6.2.5p9)
  ui = -1;
  putint((ui + 1) == 0); putchar('\n'); // 1
  putint((ui + 1) / 7);  putchar('\n'); // 0
  ui = 0x80000000;
  putint((ui * 2) / 7);   putchar('\n'); // 0
  putint((ui << 1) == 0); putchar('\n'); // 1
  putint(-ui != ui);      putchar('\n'); // 0 (-0x80000000 wraps to 0x80000000)
  ui = 0;
  putint(~ui == -1); putchar('\n'); // 1 (0xffffffff compared unsigned)

  // The value of an assignment is the value after conversion to the
  // assigned type
  putint((ui = -1) / 7);   putchar('\n'); // 613566756 (0xffffffff / 7)
  putint((c = 300) + 1);   putchar('\n'); // 45 ((char)300 is 44)
  c = 100;
  putint((c += 200) < 50); putchar('\n'); // 1 (c wraps to 44)

  // The arms of a ternary undergo the usual arithmetic conversions and the
  // common type is the type of the result.
  si = -16;
  ui = 7;
  putint((si < 0 ? si : ui) / 7); putchar('\n'); // 613566754 (true arm converted to unsigned)
  putint((si > 0 ? ui : si) / 7); putchar('\n'); // 613566754 (false arm converted to unsigned)
  putint((si < 0 ? si : ui) < 0); putchar('\n'); // 0 (unsigned comparison)

  // Returned values are converted to the function's return type
  putint(retneg() / 7); putchar('\n'); // 613566756

  return 0;
}
