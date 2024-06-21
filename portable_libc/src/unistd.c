#include "include/unistd.h"

void exit(int status) {
  /*TODO*/
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

int open(const char *pathname, int flags, mode_t mode) {
  /*TODO*/
  return 0;
}

int close(int fd) {
  /*TODO*/
  return 0;
}

ssize_t write(int fd, void *buf, size_t count) {
  /*TODO*/
  return 0;
}

ssize_t read(int fd, void *buf, size_t count) {
  /*TODO*/
  return 0;
}

off_t lseek(int fd, off_t offset, int whence) {
  /*TODO*/
  return 0;
}
