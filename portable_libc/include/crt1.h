#ifndef _CRT1_H
#define _CRT1_H

#include <stdarg.h>
#include <unistd.h>

#ifdef FLAT_INCLUDES
#include <types.h>
#else
#include <sys/types.h>
#endif

// Primitives
void exit(int status);
ssize_t read(int fd, void *buf, size_t count);
ssize_t write(int fd, void *buf, size_t count);
int open(const char *pathname, int flags, ...);
int close(int fd);
off_t lseek(int fd, off_t offset, int whence);
int unlink(const char *pathname);
int mkdir(const char *pathname, mode_t mode);
int chmod(const char *pathname, mode_t mode);
int access(const char *pathname, int amode);
char *_getcwd(char *buf, size_t size);

#endif
