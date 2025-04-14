#include "../include/signal.h"

#ifdef ADD_LIBC_STUB

int sigaction(int signum, const struct sigaction *act, struct sigaction *oldact) {
  return 0; /*TODO*/
}

int sigemptyset(sigset_t *set) {
  return 0; /*TODO*/
}

#endif
