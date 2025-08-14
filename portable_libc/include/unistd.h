#ifndef _UNISTD_H
#define _UNISTD_H

#include <sys/types.h>

typedef int mode_t;

void exit(int status);

ssize_t read(int fd, void *buf, size_t count);
ssize_t write(int fd, void *buf, size_t count);

int open(const char *pathname, int flags, ...);
int close(int fd);

// lseek whence options
#define SEEK_CUR 1
#define SEEK_END 2
#define SEEK_SET 0

off_t lseek(int fd, off_t offset, int whence);

int unlink(const char *pathname);

// Access flags to test for read, write, execute permissions
#define	R_OK	4		/* Test for read permission.  */
#define	W_OK	2		/* Test for write permission.  */
#define	X_OK	1		/* Test for execute permission.  */
#define	F_OK	0		/* Test for existence.  */

int access(const char *pathname, int amode);

char *getcwd(char *buf, size_t size);
int execvp(const char *file, char *const argv[]);

#endif
