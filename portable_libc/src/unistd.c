#include "include/unistd.h"
#include "include/crt1.h"

void exit(int status) {
  _exit(status);
}

ssize_t read(int fd, void *buf, size_t count) {
  return _read(fd, buf, count);
}

ssize_t write(int fd, void *buf, size_t count) {
  return _write(fd, buf, count);
}

int open(const char *pathname, int flags, mode_t mode) {
  return _open(pathname, flags, mode);
}

int close(int fd) {
  return _close(fd);
}

off_t lseek(int fd, off_t offset, int whence) {
  /*TODO*/
  return 0;
}

char *getcwd(char *buf, size_t size) {
  /*
  if (buf == 0)
    buf = __getcwd_buf;
  if (buf == 0)
    {
      __getcwd_buf = malloc (PATH_MAX);
      buf = __getcwd_buf;
    }
  return _getcwd (buf, size);
  */
  return 0; /*TODO*/
}
