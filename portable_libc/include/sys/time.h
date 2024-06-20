#ifndef _SYS_TIME_H
#define _SYS_TIME_H

#ifdef TODO

struct timeval {
  int tv_sec;
  int tv_usec;
};

struct timezone {
  int tz_minuteswest;
  int tz_dsttime;
};

int gettimeofday(struct timeval *tv, struct timezone *tz);

#else

#undef _SYS_TYPES_H
#include "sys/time.h"

#endif

#endif
