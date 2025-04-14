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
