#include <time.h>

#ifdef FLAT_INCLUDES
#include <time.h>
#else
#include <sys/time.h>
#endif

#ifdef ADD_LIBC_STUB

int gettimeofday(struct timeval *tv, struct timezone *tz) {
  tv->tv_sec = 0;
  tv->tv_usec = 0;
  return 0;
}

time_t time(time_t *tloc) {
  if (tloc) *tloc = 0;
  return 0;
}

struct tm *localtime (time_t const *timep) {
  static struct tm tm;
  if (timep) {
    pnut_abort("localtime: timep is not NULL");
  }
  // if (timep) {
  //   tm.tm_sec = 0;
  //   tm.tm_min = 0;
  //   tm.tm_hour = 0;
  //   tm.tm_mday = 1;
  //   tm.tm_mon = 0;
  //   tm.tm_year = 1970;
  // }
  return &tm;
}

#endif
