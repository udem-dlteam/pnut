#include <crt1.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/types.h>

int main(int argc, char **argv); /* defined in user program */

#ifndef PNUT_CC

#ifdef i386

/* This implementation assumes the OS is linux */

void exit(int status) {
  __asm__ (
    "mov   $1, %%eax\n"  /* 1 = SYS_EXIT */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : : "b" (status)
  );
  while (1) ; /* avoid noreturn warning */
}

void _start() {
  int argc;
  char **argv;
  __asm__ (
    "mov  4(%%ebp), %0\n"  /* get argc */
    "lea  8(%%ebp), %1\n"  /* get argv */
    : "=a" (argc), "=d" (argv)
  );
  exit(main(argc, argv));
}

ssize_t read(int fd, void *buf, size_t count) {
  ssize_t result;
  __asm__ (
    "mov   $3, %%eax\n"  /* 3 = SYS_READ */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd), "c" (buf), "d" (count)
  );
  return result;
}

ssize_t write(int fd, void *buf, size_t count) {
  ssize_t result;
  __asm__ (
    "mov   $4, %%eax\n"  /* 4 = SYS_WRITE */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd), "c" (buf), "d" (count)
  );
  return result;
}

int open(const char *pathname, int flags, ...) {
  va_list ap;
  va_start(ap, flags);
  int mode = va_arg(ap, int);
  va_end(ap);

  int result;
  __asm__ (
    "mov   $5, %%eax\n"  /* 5 = SYS_OPEN */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (pathname), "c" (flags), "d" (mode)
  );
  return result;
}

int close(int fd) {
  int result;
  __asm__ (
    "mov   $6, %%eax\n"  /* 6 = SYS_CLOSE */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd)
  );
  return result;
}

off_t lseek(int fd, off_t offset, int whence) {
  off_t result;
  __asm__ (
    "mov   $19, %%eax\n"  /* 8 = SYS_LSEEK */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd), "c" (offset), "d" (whence)
  );
  return result;
}

int unlink(const char *pathname) {
  int result;
  __asm__ (
    "mov   $10, %%eax\n"  /* 10 = SYS_UNLINK */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (pathname)
  );
  return result;
}

int mkdir(const char *pathname, mode_t mode) {
  int result;
  __asm__ (
    "mov   $39, %%eax\n"  /* 39 = SYS_MKDIR */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (pathname), "c" (mode)
  );
  return result;
}

int chmod(const char *pathname, mode_t mode) {
  int result;
  __asm__ (
    "mov   $15, %%eax\n"  /* 15 = SYS_CHMOD */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (pathname), "c" (mode)
  );
  return result;
}

int access(const char *pathname, int amode) {
  int result;
  __asm__ (
    "mov   $21, %%eax\n"  /* 21 = SYS_ACCESS */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (pathname), "c" (amode)
  );
  return result;
}

char *_getcwd(char *buf, size_t size) {
  char *result;
  __asm__ (
    "mov   $183, %%eax\n"  /* 79 = SYS_GETCWD */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (buf), "c" (size)
  );
  return result;
}

#else

#error Unsupported architecture

#endif

#endif
