#include "../include/unistd.h"
#include "../include/crt1.h"

/* These are assumed to be builtin

void exit(int status);
ssize_t read(int fd, void *buf, size_t count);
ssize_t write(int fd, void *buf, size_t count);
int open(const char *pathname, int flags, mode_t mode);
int close(int fd);
off_t lseek(int fd, off_t offset, int whence);
int unlink(const char *pathname);

*/

#ifdef PNUT_CC

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

#endif
