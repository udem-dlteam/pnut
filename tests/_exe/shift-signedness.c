// In C, whether >> is arithmetic or logical depends only on the (promoted)
// type of the LEFT operand; the signedness of the shift count is irrelevant.
// The result type of a shift is also the promoted left operand type, which
// matters when the shift result feeds a signedness-sensitive op (/, %, >>,
// ordered comparison).
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

void puthex(unsigned int n) {
  int sh = 28;
  unsigned int d;
  while (sh >= 0) {
    d = (n >> sh) & 0xf;
    putchar(d < 10 ? ('0' + d) : ('a' + d - 10));
    sh -= 4;
  }
}

int main() {
  int si;
  unsigned int ui;
  short ss;
  int sc = 4;          // signed shift count
  unsigned int uc = 4; // unsigned shift count

  // Signed left operand: arithmetic shift regardless of count signedness
  si = -256;
  putint(si >> sc); putchar('\n'); // -16
  putint(si >> uc); putchar('\n'); // -16

  // Unsigned left operand: logical shift regardless of count signedness
  ui = 0x80000000;
  puthex(ui >> sc); putchar('\n'); // 08000000
  puthex(ui >> uc); putchar('\n'); // 08000000

  // Compound assignment follows the lhs type
  si = -256;
  si >>= uc;
  putint(si); putchar('\n'); // -16
  ui = 0x80000000;
  ui >>= sc;
  puthex(ui); putchar('\n'); // 08000000

  // Smaller signed types promote to int and stay arithmetic
  ss = -256;
  putint(ss >> uc); putchar('\n'); // -16

  // The result type of >> is the left operand's type, so these stay signed
  si = -256;
  putint((si >> uc) >> uc); putchar('\n'); // -1
  putint((si >> uc) / sc);  putchar('\n'); // -4 (signed division)
  putint((si >> uc) % 5);   putchar('\n'); // -1 (signed remainder)
  putint((si >> uc) < 0);   putchar('\n'); // 1  (signed comparison)

  return 0;
}
