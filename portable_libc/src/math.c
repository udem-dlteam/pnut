#include <math.h>

#ifdef ADD_LIBC_STUB

double ldexp(double x, int exp) {
  pnut_abort("ldexp not implemented");
}

#endif
