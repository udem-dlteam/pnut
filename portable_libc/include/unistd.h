#ifndef _UNISTD_H
#define _UNISTD_H

#ifdef TODO

#include "include/sys/types.h"

void exit(int status);

char *getcwd(char *buf, size_t size);

int open(const char *pathname, int flags, mode_t mode);
int close(int fd);

ssize_t write(int fd, void *buf, size_t count);
ssize_t read(int fd, void *buf, size_t count);
off_t lseek(int fd, off_t offset, int whence);

#else

#undef _UNISTD_H
#include <unistd.h>

#endif

#endif
