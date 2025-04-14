#include "include/crt1.h"

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

int read(int fd, void *buf, int count) {
  int result;
  __asm__ (
    "mov   $3, %%eax\n"  /* 3 = SYS_READ */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd), "c" (buf), "d" (count)
  );
  return result;
}

int write(int fd, void *buf, int count) {
  int result;
  __asm__ (
    "mov   $4, %%eax\n"  /* 4 = SYS_WRITE */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd), "c" (buf), "d" (count)
  );
  return result;
}

int open(const char *pathname, int flags, int mode) {
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
    "mov   $5, %%eax\n"  /* 6 = SYS_CLOSE */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd)
  );
  return result;
}

int lseek(int fd, int offset, int whence) {
  int result;
  __asm__ (
    "mov   $8, %%eax\n"  /* 8 = SYS_LSEEK */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd), "c" (offset), "d" (whence)
  );
  return result;
}

#endif

#endif
#include "src/ctype.c"
#include "src/string.c"
#include "src/stdlib.c"
#include "src/stdio.c"
#include "src/setjmp.c"
#include "src/signal.c"
#include "src/time.c"
#include "src/math.c"
#include "src/unistd.c"
#ifdef USE_PORTABLE_LIBC

#include "include/string.h"
#include "include/stdio.h"
#include "include/setjmp.h"

#else

#include <string.h>
#include <stdio.h>
#include <setjmp.h>

#endif


int comp(int code) {
  /* normalize comparison results */
  return code < 0 ? -1 : code > 0 ? 1 : 0;
}

char buf[12];
jmp_buf jbuf;

int main(int argc, char **myargv) {

  int failed = 0;
  void *vp;
  char *cp;


  buf[0] = buf[1] = 'x';
  vp = memset(buf, 'z', 2);
  if (buf[0] != 'z' || buf[1] != 'z' || vp != buf)
    failed = 1;


  buf[0] = buf[1] = 'x';
  vp = memcpy(buf, "ab", 2);
  if (buf[0] != 'a' || buf[1] != 'b' || vp != buf)
    failed = 2;


  buf[0] = 'a'; buf[1] = 'b'; buf[2] = 'c';
  vp = memmove(buf, buf+1, 2);
  if (buf[0] != 'b' || buf[1] != 'c' || vp != buf)
    failed = 3;

  buf[0] = 'a'; buf[1] = 'b'; buf[2] = 'c';
  vp = memmove(buf+1, buf, 2);
  if (buf[1] != 'a' || buf[2] != 'b' || vp != buf+1)
    failed = 3;


  buf[0] = 'a'; buf[1] = 'b';
  if (comp(memcmp("ab", buf, 2)) != 0  ||
      comp(memcmp("a ", buf, 1)) != 0  ||
      comp(memcmp("a ", buf, 2)) != -1 ||
      comp(memcmp(buf, "a ", 2)) != 1)
    failed = 4;


  if (strlen("") != 0 ||
      strlen("abc") != 3)
    failed = 5;


  buf[0] = buf[1] = 'x';
  cp = strcpy(buf, "ab");
  if (buf[0] != 'a' || buf[1] != 'b' || cp != buf)
    failed = 6;


  buf[0] = 'a'; buf[1] = 'b'; buf[2] = 0; buf[3] = buf[4] = 'x';
  cp = strcat(buf, "cd");
  if (buf[0] != 'a' || buf[1] != 'b' || buf[2] != 'c' || buf[3] != 'd' || buf[4] != 0 || cp != buf)
    failed = 7;


  buf[0] = 'a'; buf[1] = 'b'; buf[2] = 'a'; buf[3] = 0;
  if (strchr(buf, 97) != buf   ||
      strchr(buf, 0)  != buf+3 ||
      strchr(buf, 32) != 0)
    failed = 8;


  buf[0] = 'a'; buf[1] = 'b'; buf[2] = 'a'; buf[3] = 0;
  if (strrchr(buf, 97) != buf+2 ||
      strrchr(buf, 0)  != buf+3 ||
      strrchr(buf, 32) != 0)
    failed = 9;


  if (comp(strcmp("abc", "abc"))  != 0  ||
      comp(strcmp("abc", "ab"))   != 1  ||
      comp(strcmp("ab", "abc"))   != -1 ||
      comp(strcmp("abc", "\377")) != -1 ||
      comp(strcmp("\377", "abc")) != 1)
    failed = 10;


  if (fprintf(stdout, "hello %d\n", -123) != 11 ||
      snprintf(buf, 12, "%03x", 42) != 3 ||
      buf[0] != '0' || buf[1] != '2' || buf[2] != 'a' || buf[3] != 0 ||
      snprintf(buf, 12, "%d", -2147483647-1) != 11 ||
      buf[0] != '-' || buf[1] != '2' || buf[2] != '1' || buf[3] != '4' ||
      buf[4] != '7' || buf[5] != '4' || buf[6] != '8' || buf[7] != '3' ||
      buf[8] != '6' || buf[9] != '4' || buf[10] != '8' || buf[11] != 0)
    failed = 11;


  if (setjmp(jbuf) != 0)
    failed = 12;


  if (argc != 2 ||
      strcmp(myargv[1], "abcdef") != 0)
    failed = 13;


  return failed;
}
