#! /bin/sh
# Once we have tcc compiled by pnut, it's time for tcc to recompile itself using
# mes's libc.

set -e -u -x

TCC_DIR=tcc-0.9.26-1147-gee75a10c
MES_DIR=mes-0.27
BUILD_DIR=build
PATCHES_DIR="files/tcc-patches"

MES_ARCH=x86
TCC_TARGET_ARCH=I386
PNUT_ARCH=i386_linux

# Like cat, but takes the output file as the first argument, a path prefix as
# the second argument and then file paths.
catm() {
  output="$1"; shift
  prefix="$1"; shift
  args=""
  for arg in "$@"; do
    args="$args $prefix/$arg"
  done
  cat $args > "$output"
}

touch () {
  printf "" > "$1"
}

mkdir -p $BUILD_DIR

# Patch TCC to be compatible with Pnut
# These patches are copied from https://github.com/fosslinux/live-bootstrap/pull/524/files
simple-patch ${TCC_DIR}/tcctools.c  $PATCHES_DIR/remove-fileopen.before $PATCHES_DIR/remove-fileopen.after \
  || echo "Failed to patch tcctools.c with remove-fileopen"
simple-patch ${TCC_DIR}/tcctools.c  $PATCHES_DIR/addback-fileopen.before $PATCHES_DIR/addback-fileopen.after \
  || echo "Failed to patch tcctools.c with addback-fileopen"
simple-patch ${TCC_DIR}/tccpp.c     $PATCHES_DIR/array_sizeof.before $PATCHES_DIR/array_sizeof.after \
  || echo "Failed to patch tccpp.c with array_sizeof"
simple-patch ${TCC_DIR}/libtcc.c    $PATCHES_DIR/error_set_jmp_enabled.before $PATCHES_DIR/error_set_jmp_enabled.after \
  || echo "Failed to patch libtcc.c with error_set_jmp_enabled"
simple-patch ${TCC_DIR}/libtcc.c    $PATCHES_DIR/sscanf_TCC_VERSION.before $PATCHES_DIR/sscanf_TCC_VERSION.after \
  || echo "Failed to patch libtcc.c with sscanf_TCC_VERSION"
simple-patch ${TCC_DIR}/tcc.h       $PATCHES_DIR/undefine_TCC_IS_NATIVE.before $PATCHES_DIR/undefine_TCC_IS_NATIVE.after \
  || echo "Failed to patch tcc.h with undefine_TCC_IS_NATIVE"

touch ${TCC_DIR}/config.h

# We can finally compile TCC with pnut-exe

./pnut-exe                                              \
  -I portable_libc/include/                             \
  -D __intptr_t_defined=1                               \
  -D BOOTSTRAP=1                                        \
  -D HAVE_LONG_LONG=0                                   \
  -D TCC_TARGET_${TCC_TARGET_ARCH}=1                    \
  -D CONFIG_SYSROOT=\"/\"                               \
  -D CONFIG_TCC_CRTPREFIX=\"$BUILD_DIR/boot0-lib\"      \
  -D CONFIG_TCC_ELFINTERP=\"/mes/loader\"               \
  -D CONFIG_TCC_SYSINCLUDEPATHS=\"$MES_DIR/include\"    \
  -D TCC_LIBGCC=\"$BUILD_DIR/boot0-lib/libc.a\"         \
  -D CONFIG_TCC_LIBTCC1_MES=0                           \
  -D CONFIG_TCCBOOT=1                                   \
  -D CONFIG_TCC_STATIC=1                                \
  -D CONFIG_USE_LIBGCC=1                                \
  -D TCC_VERSION=\"0.9.26\"                             \
  -D ONE_SOURCE=1                                       \
  -D CONFIG_TCCDIR=\"$BUILD_DIR/boot0-lib/tcc\"         \
  $TCC_DIR/tcc.c                                        \
  portable_libc/libc.c                                  \
  -o $BUILD_DIR/tcc-pnut

# Revert the patches to match live-bootstrap
simple-patch ${TCC_DIR}/tcc.h       $PATCHES_DIR/undefine_TCC_IS_NATIVE.before $PATCHES_DIR/undefine_TCC_IS_NATIVE.after \
  || echo "Failed to patch tcc.h with undefine_TCC_IS_NATIVE"
simple-patch ${TCC_DIR}/libtcc.c    $PATCHES_DIR/sscanf_TCC_VERSION.before $PATCHES_DIR/sscanf_TCC_VERSION.after \
  || echo "Failed to patch libtcc.c with sscanf_TCC_VERSION"
simple-patch ${TCC_DIR}/libtcc.c    $PATCHES_DIR/error_set_jmp_enabled.before $PATCHES_DIR/error_set_jmp_enabled.after \
  || echo "Failed to patch libtcc.c with error_set_jmp_enabled"
simple-patch ${TCC_DIR}/tccpp.c     $PATCHES_DIR/array_sizeof.before $PATCHES_DIR/array_sizeof.after \
  || echo "Failed to patch tccpp.c with array_sizeof"

# Install Mes's libc
mkdir -p $MES_DIR/include/arch
cp $MES_DIR/include/linux/${MES_ARCH}/kernel-stat.h $MES_DIR/include/arch/kernel-stat.h
cp $MES_DIR/include/linux/${MES_ARCH}/signal.h $MES_DIR/include/arch/signal.h
cp $MES_DIR/include/linux/${MES_ARCH}/syscall.h $MES_DIR/include/arch/syscall.h

# Create config.h
touch ${MES_DIR}/include/mes/config.h
cp files/mes-config.h $MES_DIR/include/mes/config.h

# Compile the mes C library for tcc-boot0

# Create unified libc file
mkdir -p "$BUILD_DIR/boot0"
catm "$BUILD_DIR/boot0/unified-libc.c" "$MES_DIR/lib" ctype/isalnum.c ctype/isalpha.c ctype/isascii.c ctype/iscntrl.c ctype/isdigit.c ctype/isgraph.c ctype/islower.c ctype/isnumber.c ctype/isprint.c ctype/ispunct.c ctype/isspace.c ctype/isupper.c ctype/isxdigit.c ctype/tolower.c ctype/toupper.c dirent/closedir.c dirent/__getdirentries.c dirent/opendir.c linux/readdir.c linux/access.c linux/brk.c linux/chdir.c linux/chmod.c linux/clock_gettime.c linux/close.c linux/dup2.c linux/dup.c linux/execve.c linux/fcntl.c linux/fork.c linux/fsync.c linux/fstat.c linux/_getcwd.c linux/getdents.c linux/getegid.c linux/geteuid.c linux/getgid.c linux/getpid.c linux/getppid.c linux/getrusage.c linux/gettimeofday.c linux/getuid.c linux/ioctl.c linux/ioctl3.c linux/kill.c linux/link.c linux/lseek.c linux/lstat.c linux/malloc.c linux/mkdir.c linux/mknod.c linux/nanosleep.c linux/_open3.c linux/pipe.c linux/_read.c linux/readlink.c linux/rename.c linux/rmdir.c linux/setgid.c linux/settimer.c linux/setuid.c linux/signal.c linux/sigprogmask.c linux/symlink.c linux/stat.c linux/time.c linux/unlink.c linux/waitpid.c linux/wait4.c linux/${MES_ARCH}-mes-gcc/_exit.c linux/${MES_ARCH}-mes-gcc/syscall.c linux/${MES_ARCH}-mes-gcc/_write.c math/ceil.c math/fabs.c math/floor.c mes/abtod.c mes/abtol.c mes/__assert_fail.c mes/assert_msg.c mes/__buffered_read.c mes/__init_io.c mes/cast.c mes/dtoab.c mes/eputc.c mes/eputs.c mes/fdgetc.c mes/fdgets.c mes/fdputc.c mes/fdputs.c mes/fdungetc.c mes/globals.c mes/itoa.c mes/ltoab.c mes/ltoa.c mes/__mes_debug.c mes/mes_open.c mes/ntoab.c mes/oputc.c mes/oputs.c mes/search-path.c mes/ultoa.c mes/utoa.c posix/alarm.c posix/buffered-read.c posix/execl.c posix/execlp.c posix/execv.c posix/execvp.c posix/getcwd.c posix/getenv.c posix/isatty.c posix/mktemp.c posix/open.c posix/pathconf.c posix/raise.c posix/sbrk.c posix/setenv.c posix/sleep.c posix/unsetenv.c posix/wait.c posix/write.c stdio/clearerr.c stdio/fclose.c stdio/fdopen.c stdio/feof.c stdio/ferror.c stdio/fflush.c stdio/fgetc.c stdio/fgets.c stdio/fileno.c stdio/fopen.c stdio/fprintf.c stdio/fputc.c stdio/fputs.c stdio/fread.c stdio/freopen.c stdio/fscanf.c stdio/fseek.c stdio/ftell.c stdio/fwrite.c stdio/getc.c stdio/getchar.c stdio/perror.c stdio/printf.c stdio/putc.c stdio/putchar.c stdio/remove.c stdio/snprintf.c stdio/sprintf.c stdio/sscanf.c stdio/ungetc.c stdio/vfprintf.c stdio/vfscanf.c stdio/vprintf.c stdio/vsnprintf.c stdio/vsprintf.c stdio/vsscanf.c stdlib/abort.c stdlib/abs.c stdlib/alloca.c stdlib/atexit.c stdlib/atof.c stdlib/atoi.c stdlib/atol.c stdlib/calloc.c stdlib/__exit.c stdlib/exit.c stdlib/free.c stdlib/mbstowcs.c stdlib/puts.c stdlib/qsort.c stdlib/realloc.c stdlib/strtod.c stdlib/strtof.c stdlib/strtol.c stdlib/strtold.c stdlib/strtoll.c stdlib/strtoul.c stdlib/strtoull.c string/bcmp.c string/bcopy.c string/bzero.c string/index.c string/memchr.c string/memcmp.c string/memcpy.c string/memmem.c string/memmove.c string/memset.c string/rindex.c string/strcat.c string/strchr.c string/strcmp.c string/strcpy.c string/strcspn.c string/strdup.c string/strerror.c string/strlen.c string/strlwr.c string/strncat.c string/strncmp.c string/strncpy.c string/strpbrk.c string/strrchr.c string/strspn.c string/strstr.c string/strupr.c stub/atan2.c stub/bsearch.c stub/chown.c stub/__cleanup.c stub/cos.c stub/ctime.c stub/exp.c stub/fpurge.c stub/freadahead.c stub/frexp.c stub/getgrgid.c stub/getgrnam.c stub/getlogin.c stub/getpgid.c stub/getpgrp.c stub/getpwnam.c stub/getpwuid.c stub/gmtime.c stub/ldexp.c stub/localtime.c stub/log.c stub/mktime.c stub/modf.c stub/mprotect.c stub/pclose.c stub/popen.c stub/pow.c stub/putenv.c stub/rand.c stub/realpath.c stub/rewind.c stub/setbuf.c stub/setgrent.c stub/setlocale.c stub/setvbuf.c stub/sigaction.c stub/sigaddset.c stub/sigblock.c stub/sigdelset.c stub/sigemptyset.c stub/sigsetmask.c stub/sin.c stub/sys_siglist.c stub/system.c stub/sqrt.c stub/strftime.c stub/times.c stub/ttyname.c stub/umask.c stub/utime.c ${MES_ARCH}-mes-gcc/setjmp.c

# Compile libc for boot0
mkdir -p "$BUILD_DIR/boot0-lib"
mkdir -p "$BUILD_DIR/boot0-lib/tcc"

# crt1.o
$BUILD_DIR/tcc-pnut -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crt1.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crt1.c
# For x86, we must also create crtn.o and crti.o
# crtn.o
$BUILD_DIR/tcc-pnut -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $$MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crtn.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crtn.c
# crti.o
$BUILD_DIR/tcc-pnut -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crti.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crti.c

sha256sum $BUILD_DIR/boot0-lib/crt1.o $BUILD_DIR/boot0-lib/crtn.o $BUILD_DIR/boot0-lib/crti.o

# libc+gcc.a
$BUILD_DIR/tcc-pnut -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/unified-libc.o $BUILD_DIR/boot0/unified-libc.c
$BUILD_DIR/tcc-pnut -ar cr $BUILD_DIR/boot0-lib/libc.a $BUILD_DIR/boot0-lib/unified-libc.o

# libtcc1.a
$BUILD_DIR/tcc-pnut -c -D HAVE_CONFIG_H=1 -D HAVE_LONG_LONG=1 -D HAVE_FLOAT=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/libtcc1.o $MES_DIR/lib/libtcc1.c
$BUILD_DIR/tcc-pnut -ar cr $BUILD_DIR/boot0-lib/tcc/libtcc1.a $BUILD_DIR/boot0-lib/libtcc1.o

# libgetopt.a
$BUILD_DIR/tcc-pnut -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} $MES_DIR/lib/posix/getopt.c -o $BUILD_DIR/boot0-lib/getopt.o
$BUILD_DIR/tcc-pnut -ar cr $BUILD_DIR/boot0-lib/libgetopt.a $BUILD_DIR/boot0-lib/getopt.o

# We can now compile tcc-boot0
$BUILD_DIR/tcc-pnut \
    -g \
    -v \
    -static \
    -o $BUILD_DIR/tcc-boot0 \
    -D BOOTSTRAP=1 \
    -D HAVE_FLOAT=1 \
    -D HAVE_BITFIELD=1 \
    -D HAVE_LONG_LONG=1 \
    -D HAVE_SETJMP=1 \
    -I $MES_DIR/include \
    -D TCC_TARGET_${TCC_TARGET_ARCH}=1 \
    -D CONFIG_TCCDIR=\"$BUILD_DIR/boot0-lib/tcc\" \
    -D CONFIG_TCC_CRTPREFIX=\"$BUILD_DIR/boot0-lib\" \
    -D CONFIG_TCC_LIBPATHS=\"$BUILD_DIR/boot0-lib:$BUILD_DIR/boot0-lib/tcc\" \
    -D CONFIG_TCC_SYSINCLUDEPATHS=\"$MES_DIR/include\" \
    -D TCC_LIBGCC=\"$BUILD_DIR/boot0-lib/libc.a\" \
    -D TCC_LIBTCC1=\"libtcc1.a\" \
    -D CONFIG_TCC_ELFINTERP=\"/mes/loader\" \
    -D CONFIG_TCCBOOT=1 \
    -D CONFIG_TCC_STATIC=1 \
    -D CONFIG_USE_LIBGCC=1 \
    -D TCC_VERSION=\"0.9.26\" \
    -D ONE_SOURCE=1 \
    -L $BUILD_DIR/boot0-lib \
    $TCC_DIR/tcc.c

sha256sum $BUILD_DIR/tcc-pnut $BUILD_DIR/tcc-boot0

# Recompile libc: crt{1,n,i}, libtcc.a, libc.a using tcc-boot0
$BUILD_DIR/tcc-boot0 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crt1.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crt1.c
$BUILD_DIR/tcc-boot0 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crtn.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crtn.c
$BUILD_DIR/tcc-boot0 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crti.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crti.c

$BUILD_DIR/tcc-boot0 -c -D HAVE_CONFIG_H=1 -D HAVE_LONG_LONG=1 -D HAVE_FLOAT=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/libtcc1.o $MES_DIR/lib/libtcc1.c
$BUILD_DIR/tcc-boot0 -ar cr $BUILD_DIR/boot0-lib/tcc/libtcc1.a $BUILD_DIR/boot0-lib/libtcc1.o

$BUILD_DIR/tcc-boot0 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/unified-libc.o $BUILD_DIR/boot0/unified-libc.c
$BUILD_DIR/tcc-boot0 -ar cr $BUILD_DIR/boot0-lib/libc.a $BUILD_DIR/boot0-lib/unified-libc.o

$BUILD_DIR/tcc-boot0 \
    -g \
    -v \
    -static \
    -o $BUILD_DIR/tcc-boot1 \
    -D BOOTSTRAP=1 \
    -D HAVE_FLOAT=1 \
    -D HAVE_BITFIELD=1 \
    -D HAVE_LONG_LONG=1 \
    -D HAVE_SETJMP=1 \
    -I $MES_DIR/include \
    -D TCC_TARGET_${TCC_TARGET_ARCH}=1 \
    -D CONFIG_TCCDIR=\"$BUILD_DIR/boot0-lib/tcc\" \
    -D CONFIG_TCC_CRTPREFIX=\"$BUILD_DIR/boot0-lib\" \
    -D CONFIG_TCC_LIBPATHS=\"$BUILD_DIR/boot0-lib:$BUILD_DIR/boot0-lib/tcc\" \
    -D CONFIG_TCC_SYSINCLUDEPATHS=\"$MES_DIR/include\" \
    -D TCC_LIBGCC=\"$BUILD_DIR/boot0-lib/libc.a\" \
    -D TCC_LIBTCC1=\"libtcc1.a\" \
    -D CONFIG_TCC_ELFINTERP=\"/mes/loader\" \
    -D CONFIG_TCCBOOT=1 \
    -D CONFIG_TCC_STATIC=1 \
    -D CONFIG_USE_LIBGCC=1 \
    -D TCC_VERSION=\"0.9.26\" \
    -D ONE_SOURCE=1 \
    -L $BUILD_DIR/boot0-lib \
    $TCC_DIR/tcc.c

# Recompile libc: crt{1,n,i}, libtcc.a, libc.a using tcc-boot1
$BUILD_DIR/tcc-boot1 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crt1.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crt1.c
$BUILD_DIR/tcc-boot1 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crtn.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crtn.c
$BUILD_DIR/tcc-boot1 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crti.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crti.c

$BUILD_DIR/tcc-boot1 -c -D HAVE_CONFIG_H=1 -D HAVE_LONG_LONG=1 -D HAVE_FLOAT=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/libtcc1.o $MES_DIR/lib/libtcc1.c
$BUILD_DIR/tcc-boot1 -ar cr $BUILD_DIR/boot0-lib/tcc/libtcc1.a $BUILD_DIR/boot0-lib/libtcc1.o

$BUILD_DIR/tcc-boot1 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/unified-libc.o $BUILD_DIR/boot0/unified-libc.c
$BUILD_DIR/tcc-boot1 -ar cr $BUILD_DIR/boot0-lib/libc.a $BUILD_DIR/boot0-lib/unified-libc.o

$BUILD_DIR/tcc-boot1 \
    -g \
    -v \
    -static \
    -o $BUILD_DIR/tcc-boot2 \
    -D BOOTSTRAP=1 \
    -D HAVE_FLOAT=1 \
    -D HAVE_BITFIELD=1 \
    -D HAVE_LONG_LONG=1 \
    -D HAVE_SETJMP=1 \
    -I $MES_DIR/include \
    -D TCC_TARGET_${TCC_TARGET_ARCH}=1 \
    -D CONFIG_TCCDIR=\"$BUILD_DIR/boot0-lib/tcc\" \
    -D CONFIG_TCC_CRTPREFIX=\"$BUILD_DIR/boot0-lib\" \
    -D CONFIG_TCC_LIBPATHS=\"$BUILD_DIR/boot0-lib:$BUILD_DIR/boot0-lib/tcc\" \
    -D CONFIG_TCC_SYSINCLUDEPATHS=\"$MES_DIR/include\" \
    -D TCC_LIBGCC=\"$BUILD_DIR/boot0-lib/libc.a\" \
    -D TCC_LIBTCC1=\"libtcc1.a\" \
    -D CONFIG_TCC_ELFINTERP=\"/mes/loader\" \
    -D CONFIG_TCCBOOT=1 \
    -D CONFIG_TCC_STATIC=1 \
    -D CONFIG_USE_LIBGCC=1 \
    -D TCC_VERSION=\"0.9.26\" \
    -D ONE_SOURCE=1 \
    -L $BUILD_DIR/boot0-lib \
    $TCC_DIR/tcc.c

sha256sum $BUILD_DIR/boot0-lib/crt1.o $BUILD_DIR/boot0-lib/crtn.o $BUILD_DIR/boot0-lib/crti.o
sha256sum $BUILD_DIR/tcc-boot0 $BUILD_DIR/tcc-boot1 $BUILD_DIR/tcc-boot2

# Make sure we've reached a fixed point

$BUILD_DIR/tcc-boot2 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crt1.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crt1.c
$BUILD_DIR/tcc-boot2 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crtn.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crtn.c
$BUILD_DIR/tcc-boot2 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/crti.o $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/crti.c

$BUILD_DIR/tcc-boot2 -c -D HAVE_CONFIG_H=1 -D HAVE_LONG_LONG=1 -D HAVE_FLOAT=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/libtcc1.o $MES_DIR/lib/libtcc1.c
$BUILD_DIR/tcc-boot2 -ar cr $BUILD_DIR/boot0-lib/tcc/libtcc1.a $BUILD_DIR/boot0-lib/libtcc1.o

$BUILD_DIR/tcc-boot2 -c -D HAVE_CONFIG_H=1 -I $MES_DIR/include -I $MES_DIR/include/linux/${MES_ARCH} -o $BUILD_DIR/boot0-lib/unified-libc.o $BUILD_DIR/boot0/unified-libc.c
$BUILD_DIR/tcc-boot2 -ar cr $BUILD_DIR/boot0-lib/libc.a $BUILD_DIR/boot0-lib/unified-libc.o

$BUILD_DIR/tcc-boot2 \
    -g \
    -v \
    -static \
    -o $BUILD_DIR/tcc-boot3 \
    -D BOOTSTRAP=1 \
    -D HAVE_FLOAT=1 \
    -D HAVE_BITFIELD=1 \
    -D HAVE_LONG_LONG=1 \
    -D HAVE_SETJMP=1 \
    -I $MES_DIR/include \
    -D TCC_TARGET_${TCC_TARGET_ARCH}=1 \
    -D CONFIG_TCCDIR=\"$BUILD_DIR/boot0-lib/tcc\" \
    -D CONFIG_TCC_CRTPREFIX=\"$BUILD_DIR/boot0-lib\" \
    -D CONFIG_TCC_LIBPATHS=\"$BUILD_DIR/boot0-lib:$BUILD_DIR/boot0-lib/tcc\" \
    -D CONFIG_TCC_SYSINCLUDEPATHS=\"$MES_DIR/include\" \
    -D TCC_LIBGCC=\"$BUILD_DIR/boot0-lib/libc.a\" \
    -D TCC_LIBTCC1=\"libtcc1.a\" \
    -D CONFIG_TCC_ELFINTERP=\"/mes/loader\" \
    -D CONFIG_TCCBOOT=1 \
    -D CONFIG_TCC_STATIC=1 \
    -D CONFIG_USE_LIBGCC=1 \
    -D TCC_VERSION=\"0.9.26\" \
    -D ONE_SOURCE=1 \
    -L $BUILD_DIR/boot0-lib \
    $TCC_DIR/tcc.c

sha256sum $BUILD_DIR/boot0-lib/crt1.o $BUILD_DIR/boot0-lib/crtn.o $BUILD_DIR/boot0-lib/crti.o
sha256sum $BUILD_DIR/boot0-lib/libtcc1.o $BUILD_DIR/boot0-lib/tcc/libtcc1.a $BUILD_DIR/boot0-lib/unified-libc.o $BUILD_DIR/boot0-lib/libc.a
sha256sum $BUILD_DIR/tcc-boot0 $BUILD_DIR/tcc-boot1 $BUILD_DIR/tcc-boot2 $BUILD_DIR/tcc-boot3 # Fixed point!
