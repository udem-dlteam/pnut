#ifndef _CRT1_H
#define _CRT1_H

void _exit(int status);
int _read(int fd, void *buf, int count);
int _write(int fd, void *buf, int count);
int _open(const char *pathname, int flags, int mode);
int _close(int fd);
int _lseek(int fd, int offset, int whence);
int _unlink(const char *pathname);

#endif
