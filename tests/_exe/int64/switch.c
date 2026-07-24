// switch/case on a long long value.
#include "int64_util.h"

void classify(long long x) {
  switch (x) {
    case 0LL:
      showint(0x0);
      break;
    case 1LL:
    case 2LL:
      showint(0x12);
      break;
    case 0x100000000LL: // crosses the 32-bit boundary
      showint(0x33);
      break;
    default:
      showint(0x44);
      break;
  }
}

int main() {
  long long i;

  classify(0);
  classify(1);
  classify(2);
  classify(0x100000000LL);
  classify(3);

  // case constants narrower than the switch's long long operand still widen
  // correctly, and fallthrough/no-default behave like the scalar switch.
  switch ((long long)5) {
    case 5:
      showint(0x55);
    case 6:
      showint(0x66);
      break;
    case 7:
      showint(0x77);
      break;
  }

  switch ((long long)9) {
    case 1: showint(0x11); break;
  }
  showint(0x99); // no default, no match: falls through untouched

  // switch inside a loop, to exercise the operand staying live across cases
  // evaluated multiple times.
  for (i = 0; i < 3; ++i) {
    switch (i) {
      case 0: showint(0xa0); break;
      case 1: showint(0xa1); break;
      default: showint(0xaa); break;
    }
  }

  return 0;
}
