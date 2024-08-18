#ifndef _UNISTD_H
#define _UNISTD_H

#include "sys/types.h"

typedef int mode_t;

void exit(int status);

ssize_t read(int fd, void *buf, size_t count);
ssize_t write(int fd, void *buf, size_t count);

int open(const char *pathname, int flags, mode_t mode);
int close(int fd);

off_t lseek(int fd, off_t offset, int whence);

char *getcwd(char *buf, size_t size);

#endif
