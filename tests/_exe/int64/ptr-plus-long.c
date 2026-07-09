// Pointer arithmetic where the integer operand is a 64-bit.
#include "int64_util.h"

char buf[64];
int main() {
  unsigned long c = 5;
  long d = 3;
  char *p = buf + c;        // pointer + unsigned long(64-bit)
  *p = 'X';
  char *q = p - d;          // pointer - long(64-bit)
  *q = 'Y';
  showint(p - buf);         // 5
  showint(q - buf);         // 2
  showint(buf[5] == 'X');   // 1
  showint(buf[2] == 'Y');   // 1
  long long i = 7;
  showint((int)(buf + i - buf)); // 7  (pointer + long long)
  return 0;
}
