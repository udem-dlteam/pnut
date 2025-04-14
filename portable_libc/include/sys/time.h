#ifndef _SYS_TIME_H
#define _SYS_TIME_H

typedef long __time32_t;
typedef __time32_t time_t;

typedef long suseconds_t;

struct timeval {
  time_t      tv_sec;     /* seconds */
  suseconds_t tv_usec;    /* microseconds */
};

int gettimeofday(struct timeval *tv, void *tz);

#endif
