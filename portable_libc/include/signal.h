#ifndef _SIGNAL_H
#define _SIGNAL_H

struct sigaction {
  int dummy;
};

typedef int sigset_t;

int sigaction(int signum, const struct sigaction *act, struct sigaction *oldact);
int sigemptyset(sigset_t *set);

#endif
