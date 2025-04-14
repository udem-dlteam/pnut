#ifndef _TIME_H
#define _TIME_H

#include <sys/time.h>

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

time_t time(time_t *tloc);
struct tm *localtime (time_t const *timep);

#endif
