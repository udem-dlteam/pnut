#ifndef _UNISTD_H
#define _UNISTD_H

#include "sys/types.h"

typedef int mode_t;

void exit(int status);

ssize_t read(int fd, void *buf, size_t count);
ssize_t write(int fd, void *buf, size_t count);

int open(const char *pathname, int flags, mode_t mode);
int close(int fd);

#define SEEK_CUR 1
#define SEEK_END 2
#define SEEK_SET 0

off_t lseek(int fd, off_t offset, int whence);
int unlink(const char *pathname);

char *getcwd(char *buf, size_t size);
int execvp(const char *__file, char **__argv);

int unlink(const char *pathname);
int execvp(const char *__file, char **__argv);

#include "../src/unistd.c"

#endif
