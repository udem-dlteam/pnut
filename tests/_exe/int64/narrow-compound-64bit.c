// A narrower-than-64-bit lvalue compound-assigned a 64-bit rhs (tcc's init_putv
// does `*(char*)ptr |= (c.i & bit_mask) << bit_pos` with a `long long` bit_mask).
#include "int64_util.h"
char  cmem[8];
short smem[4];
int main() {
  unsigned long long bm = -1LL;
  int v = 0xABCD5;
  char *cp = cmem;
  short *sp = smem;
  *cp |= (v & bm) << 0;   showint((unsigned char)cmem[0]);   // 0xd5
  *sp |= (v & bm) << 0;   showint((unsigned short)smem[0]);  // 0xbcd5
  int x = 4;
  x += (v & bm);          showint(x);                        // 0xabcd9
  char c2 = 1;
  c2 += (long long)0x1FF; showint((unsigned char)c2);        // 0x00 (1+0xff)
  return 0;
}
