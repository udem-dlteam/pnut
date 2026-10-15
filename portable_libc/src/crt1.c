#include <crt1.h>
#include <stdarg.h>
#include <unistd.h>

#ifdef FLAT_INCLUDES
#include <types.h>
#else
#include <sys/types.h>
#endif

int main(int argc, char **argv); /* defined in user program */

#ifndef PNUT_CC

#ifdef i386

/* This implementation assumes the OS is linux */

/* i386 Linux syscall convention:
    eax = syscall number
    ebx, ecx, edx, esi, edi, ebp = arguments 1-6
    int 0x80
    return value in eax
*/

void exit(int status) {
  __asm__ (
    "mov   $1, %%eax\n"  /* 1 = SYS_EXIT */
    ".byte 0xcd,0x80\n"  /* int 0x80 (system call) */
    : : "b" (status) : "eax"
  );
  while (1) ; /* avoid noreturn warning */
}

/* Reads argc and argv from the stack relative to %ebp: this requires the
   compiler to set up a frame pointer (i.e. not to use -fomit-frame-pointer). */
void _start() {
  int argc;
  char **argv;
  __asm__ (
    "mov  4(%%ebp), %0\n" /* get argc */
    "lea  8(%%ebp), %1\n" /* get argv */
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
    "mov   $19, %%eax\n"  /* 19 = SYS_LSEEK */
    ".byte 0xcd,0x80\n"   /* int 0x80 (system call) */
    : "=a" (result) : "b" (fd), "c" (offset), "d" (whence)
  );
  return result;
}

int unlink(const char *pathname) {
  int result;
  __asm__ (
    "mov   $10, %%eax\n"  /* 10 = SYS_UNLINK */
    ".byte 0xcd,0x80\n"   /* int 0x80 (system call) */
    : "=a" (result) : "b" (pathname)
  );
  return result;
}

int mkdir(const char *pathname, mode_t mode) {
  int result;
  __asm__ (
    "mov   $39, %%eax\n"  /* 39 = SYS_MKDIR */
    ".byte 0xcd,0x80\n"   /* int 0x80 (system call) */
    : "=a" (result) : "b" (pathname), "c" (mode)
  );
  return result;
}

int chmod(const char *pathname, mode_t mode) {
  int result;
  __asm__ (
    "mov   $15, %%eax\n"  /* 15 = SYS_CHMOD */
    ".byte 0xcd,0x80\n"   /* int 0x80 (system call) */
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
  int result;
  __asm__ (
    "mov   $183, %%eax\n" /* 183 = SYS_GETCWD */
    ".byte 0xcd,0x80\n"   /* int 0x80 (system call) */
    : "=a" (result) : "b" (buf), "c" (size)
  );
  if (result < 0) return 0;
  return buf;
}

#elif defined(__x86_64__)

/* This implementation assumes the OS is linux */

/* x86-64 Linux syscall convention:
    rax = syscall number
    rdi, rsi, rdx, r10, r8, r9 = arguments 1-6: "D", "S", "d", "r10", "r8", "r9"
                                                input constraints
    syscall                                   : clobbers rcx and r11
    return value in rax                       : "=a" output constraint
*/

void exit(int status) {
  __asm__ (
    "mov $60, %%eax\n"    /* 60 = SYS_exit */
    "syscall\n"
    : : "D" (status) : "rcx", "r11"
  );
  while (1) ; /* avoid noreturn warning */
}

/* Reads argc and argv from the stack relative to %rbp: this requires the
   compiler to set up a frame pointer (i.e. not to use -fomit-frame-pointer). */
void _start() {
  __asm__ (
    "mov  8(%rbp), %rdi\n"  /* argc, above the return address */
    "lea  16(%rbp), %rsi\n" /* argv */
    "and  $-16, %rsp\n"     /* align the stack before calling main */
    "call main\n"
    "mov  %eax, %edi\n"     /* main's return value becomes the exit status */
    "mov  $60, %eax\n"      /* 60 = SYS_exit */
    "syscall\n"
  );
  while (1) ; /* avoid noreturn warning */
}

ssize_t read(int fd, void *buf, size_t count) {
  ssize_t result;
  __asm__ (
    "mov $0, %%eax\n"     /* 0 = SYS_read */
    "syscall\n"
    : "=a" (result) : "D" (fd), "S" (buf), "d" (count) : "rcx", "r11"
  );
  return result;
}

ssize_t write(int fd, void *buf, size_t count) {
  ssize_t result;
  __asm__ (
    "mov $1, %%eax\n"     /* 1 = SYS_write */
    "syscall\n"
    : "=a" (result) : "D" (fd), "S" (buf), "d" (count) : "rcx", "r11"
  );
  return result;
}

int open(const char *pathname, int flags, ...) {
  /* The mode is passed as the 3rd argument, in %rdx, so it doesn't have to be
     read with va_arg. */
  int result;
  __asm__ (
    "mov $2, %%eax\n"     /* 2 = SYS_open */
    "syscall\n"
    : "=a" (result) : : "rcx", "r11"
  );
  return result;
}

int close(int fd) {
  int result;
  __asm__ (
    "mov $3, %%eax\n"     /* 3 = SYS_close */
    "syscall\n"
    : "=a" (result) : "D" (fd) : "rcx", "r11"
  );
  return result;
}

off_t lseek(int fd, off_t offset, int whence) {
  off_t result;
  __asm__ (
    "mov $8, %%eax\n"     /* 8 = SYS_lseek */
    "syscall\n"
    : "=a" (result) : "D" (fd), "S" (offset), "d" (whence) : "rcx", "r11"
  );
  return result;
}

int unlink(const char *pathname) {
  int result;
  __asm__ (
    "mov $87, %%eax\n"    /* 87 = SYS_unlink */
    "syscall\n"
    : "=a" (result) : "D" (pathname) : "rcx", "r11"
  );
  return result;
}

int mkdir(const char *pathname, mode_t mode) {
  int result;
  __asm__ (
    "mov $83, %%eax\n"    /* 83 = SYS_mkdir */
    "syscall\n"
    : "=a" (result) : "D" (pathname), "S" (mode) : "rcx", "r11"
  );
  return result;
}

int chmod(const char *pathname, mode_t mode) {
  int result;
  __asm__ (
    "mov $90, %%eax\n"    /* 90 = SYS_chmod */
    "syscall\n"
    : "=a" (result) : "D" (pathname), "S" (mode) : "rcx", "r11"
  );
  return result;
}

int access(const char *pathname, int amode) {
  int result;
  __asm__ (
    "mov $21, %%eax\n"    /* 21 = SYS_access */
    "syscall\n"
    : "=a" (result) : "D" (pathname), "S" (amode) : "rcx", "r11"
  );
  return result;
}

char *_getcwd(char *buf, size_t size) {
  /* The getcwd system call returns the length of the path on success, or a
     negative error code on failure, not the buffer itself. */
  long result;
  __asm__ (
    "mov $79, %%eax\n"    /* 79 = SYS_getcwd */
    "syscall\n"
    : "=a" (result) : "D" (buf), "S" (size) : "rcx", "r11"
  );
  if (result < 0) return 0;
  return buf;
}

#elif defined(__aarch64__)

/* This implementation assumes the OS is linux */

/* aarch64 Linux syscall convention:
    x8 = syscall number
    x0-x5 = arguments 1-6
    svc #0
    return value in x0

   TCC's aarch64 backend has no inline assembly support (its assembler is only
   implemented for i386/x86-64/arm), and the data segment of the ELF files it
   produces is not executable. The trampoline below is therefore raw aarch64
   machine code, put in the text section with a section attribute so that it
   can be called through a function pointer. */

typedef long (*syscall_fn)(long, long, long, long, long, long, long);

__attribute__((section(".text")))
static const unsigned int syscall_tramp[9] = {
  0xaa0003e8, /* mov x8, x0    syscall number */
  0xaa0103e0, /* mov x0, x1    argument 1 */
  0xaa0203e1, /* mov x1, x2    argument 2 */
  0xaa0303e2, /* mov x2, x3    argument 3 */
  0xaa0403e3, /* mov x3, x4    argument 4 */
  0xaa0503e4, /* mov x4, x5    argument 5 */
  0xaa0603e5, /* mov x5, x6    argument 6 */
  0xd4000001, /* svc #0 */
  0xd65f03c0, /* ret */
};

/* The syscall number comes first and the trampoline shifts the arguments down
   by one register, so that a syscall never needs more than 6 arguments. */
static long syscall6(long n, long a1, long a2, long a3, long a4, long a5) {
  return ((syscall_fn) &syscall_tramp[0])(n, a1, a2, a3, a4, a5, 0);
}

#define AT_FDCWD (-100)

void exit(int status) {
  syscall6(93 /* __NR_exit */, status, 0, 0, 0, 0);
  while (1) ; /* avoid noreturn warning */
}

/* Every function that TCC generates starts by pushing x29/x30 and reserving
   the 224 byte register save area used by va_start (see gfunc_prolog in
   arm64-gen.c), so the stack pointer at function entry is always
   __builtin_frame_address(0) + 224. When the process starts, the kernel puts
   argc at that address, followed by argv. */
void _start() {
  int argc;
  char **argv;
  long *entry_sp = (long *) ((char *) __builtin_frame_address(0) + 224);
  argc = (int) entry_sp[0];
  argv = (char **) &entry_sp[1];
  exit(main(argc, argv));
}

ssize_t read(int fd, void *buf, size_t count) {
  return syscall6(63 /* __NR_read */, fd, (long) buf, (long) count, 0, 0);
}

ssize_t write(int fd, void *buf, size_t count) {
  return syscall6(64 /* __NR_write */, fd, (long) buf, (long) count, 0, 0);
}

int open(const char *pathname, int flags, ...) {
  /* aarch64 has no open syscall, only openat, and the mode is optional. */
  va_list ap;
  long mode = 0;
  va_start(ap, flags);
  mode = va_arg(ap, int);
  va_end(ap);
  return syscall6(56 /* __NR_openat */, AT_FDCWD, (long) pathname, flags,
                  mode, 0);
}

int close(int fd) {
  return syscall6(57 /* __NR_close */, fd, 0, 0, 0, 0);
}

off_t lseek(int fd, off_t offset, int whence) {
  return syscall6(62 /* __NR_lseek */, fd, (long) offset, whence, 0, 0);
}

/* aarch64 only has the *at() variants of these system calls, which all take
   a directory file descriptor as their first argument. */

int unlink(const char *pathname) {
  return syscall6(35 /* __NR_unlinkat */, AT_FDCWD, (long) pathname, 0, 0, 0);
}

int mkdir(const char *pathname, mode_t mode) {
  return syscall6(34 /* __NR_mkdirat */, AT_FDCWD, (long) pathname, mode, 0, 0);
}

int chmod(const char *pathname, mode_t mode) {
  return syscall6(53 /* __NR_fchmodat */, AT_FDCWD, (long) pathname, mode, 0, 0);
}

int access(const char *pathname, int amode) {
  return syscall6(48 /* __NR_faccessat */, AT_FDCWD, (long) pathname, amode, 0, 0);
}

char *_getcwd(char *buf, size_t size) {
  /* The getcwd system call returns the length of the path on success, or a
     negative error code on failure, not the buffer itself. */
  long result = syscall6(17 /* __NR_getcwd */, (long) buf, (long) size, 0, 0, 0);
  if (result < 0) return 0;
  return buf;
}

#else

#error Unsupported architecture

#endif

#endif
