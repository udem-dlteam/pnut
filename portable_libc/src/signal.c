#include <pnut_lib.h>
#include <signal.h>

#ifdef ADD_LIBC_STUB

int sigaction(int signum, const struct sigaction *act, struct sigaction *oldact) {
  pnut_abort("sigaction not implemented");
  return 0;
}

int sigemptyset(sigset_t *set) {
  pnut_abort("sigemptyset not implemented");
  return 0;
}

#endif
