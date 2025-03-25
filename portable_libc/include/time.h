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

struct tm {
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;
};

int gettimeofday(struct timeval *tv, struct timezone *tz);

typedef long __time32_t;
typedef __time32_t time_t;

time_t time (time_t * tloc);
struct tm *localtime (time_t const *timep);

#endif
