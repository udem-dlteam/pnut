#include "include/crt1.h"

int main(int argc, char **argv); /* defined in user program */

#ifdef PNUT_CC

/* these are builtin operations of pnut

void _exit(int status);
int _read(int fd, void *buf, int count);
int _write(int fd, void *buf, int count);
int _open(const char *pathname, int flags, int mode);
int _close(int fd);

*/

#else

#ifdef i386

/* This implementation assumes the OS is linux */

void _start() {
  int argc;
  char **argv;
  __asm__ (
    "mov  4(%%ebp), %0\n"  /* get argc */
    "lea  8(%%ebp), %1\n"  /* get argv */
    : "=a" (argc), "=d" (argv)
  );
  _exit(main(argc, argv));
}

void _exit(int status) {
  __asm__ (
    "mov   $1, %%eax\n"  /* 1 = SYS_EXIT */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : : "b" (status)
  );
  while (1) ; /* avoid noreturn warning */
}

int _read(int fd, void *buf, int count) {
  int result;
  __asm__ (
    "mov   $3, %%eax\n"  /* 3 = SYS_READ */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd), "c" (buf), "d" (count)
  );
  return result;
}

int _write(int fd, void *buf, int count) {
  int result;
  __asm__ (
    "mov   $4, %%eax\n"  /* 4 = SYS_WRITE */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd), "c" (buf), "d" (count)
  );
  return result;
}

int _open(const char *pathname, int flags, int mode) {
  int result;
  __asm__ (
    "mov   $5, %%eax\n"  /* 5 = SYS_OPEN */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (pathname), "c" (flags), "d" (mode)
  );
  return result;
}

int _close(int fd) {
  int result;
  __asm__ (
    "mov   $5, %%eax\n"  /* 6 = SYS_CLOSE */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd)
  );
  return result;
}

#endif

#endif
