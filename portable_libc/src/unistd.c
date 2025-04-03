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
int mkdir(const char *pathname, mode_t mode);
int chmod(const char *pathname, mode_t mode);
int access(const char *pathname, int amode);

*/

#ifdef PNUT_CC

#define	R_OK	4		/* Test for read permission.  */
#define	W_OK	2		/* Test for write permission.  */
#define	X_OK	1		/* Test for execute permission.  */
#define	F_OK	0		/* Test for existence.  */

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
