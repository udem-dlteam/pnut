#include "include/sys/time.h"

int gettimeofday(struct timeval *tv, struct timezone *tz) {
#ifndef PNUT_CC
  tv->tv_sec = 0;
  tv->tv_usec = 0;
#endif
  return 0;
}
