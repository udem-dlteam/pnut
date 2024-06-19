#ifndef _SIGNAL_H
#define _SIGNAL_H

#ifdef TODO

struct sigaction {
  int dummy;
};

typedef int sigset_t;

int sigaction(int signum, const struct sigaction *act, struct sigaction *oldact);
int sigemptyset(sigset_t *set);

#else

#undef _SIGNAL_H
#include <signal.h>

#endif

#endif
