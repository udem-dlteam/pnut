#ifndef _SYS_TIME_H
#define _SYS_TIME_H

struct timeval {
  int tv_sec;
  int tv_usec;
};

struct timezone {
  int tz_minuteswest;
  int tz_dsttime;
};

int gettimeofday(struct timeval *tv, struct timezone *tz);

#endif
