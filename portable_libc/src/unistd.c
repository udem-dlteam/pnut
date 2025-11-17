#include <pnut_lib.h>
#include <unistd.h>
#include <crt1.h>

/* These are assumed to be builtin:
  void exit(int status);
  ssize_t read(int fd, void *buf, size_t count);
  ssize_t write(int fd, void *buf, size_t count);
  int open(const char *pathname, int flags, mode_t mode);
  int close(int fd);
  off_t lseek(int fd, off_t offset, int whence);
  int unlink(const char *pathname);
  int mkdir(const char *pathname, mode_t mode);
  int chmod(const char *pathname, mode_t mode);
  int access(const char *pathname, int amode);
*/

char *getcwd(char *buf, size_t size) {
#ifdef PNUT_CC
  return NULL; // Not implemented
#else
  if (buf == 0) {
    pnut_abort("getcwd: buffer is empty");
    return NULL;
  }
  return _getcwd(buf, size);
#endif
}

int execvp(const char *file, char *const argv[]) {
  pnut_abort("sigemptyset not implemented");
  return 0;
}
