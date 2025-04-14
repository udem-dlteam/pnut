#include "../include/sys/time.h"

int gettimeofday(struct timeval *tv, struct timezone *tz) {
  tv->tv_sec = 0;
  tv->tv_usec = 0;
  return 0;
}
