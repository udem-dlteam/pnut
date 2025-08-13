#! /bin/sh
set -e

TEMP_DIR=build/tcc
TCC_DIR=../tcc-0.9.26-delete-me-maybe
MES_DIR=../mes-0.27

MES_ARCH=x86
TCC_TARGET_ARCH=I386
PNUT_ARCH=i386_linux

: ${PNUT_OPTIONS:=} # Default to empty options

PNUT_EXE_OPTIONS="$PNUT_OPTIONS -DBOOTSTRAP_LONG -Dtarget_$PNUT_ARCH -DUNDEFINED_LABELS_ARE_RUNTIME_ERRORS -DENABLE_PNUT_INLINE_INTERRUPT -DONE_PASS_GENERATOR"
PNUT_SH_OPTIONS="$PNUT_OPTIONS -DRT_NO_INIT_GLOBALS -Dsh"
PNUT_SH_OPTIONS_FAST="$PNUT_SH_OPTIONS -DSH_SAVE_VARS_WITH_SET -DOPTIMIZE_CONSTANT_PARAM"

if [ ! -d "$TEMP_DIR" ]; then mkdir -p "$TEMP_DIR"; fi

# Parse the arguments

use_gcc=0               # Whether to use gcc to compile the first version of TCC, pnut-exe is used otherwise
shell=                  # Defined if doing the full bootstrap using pnut.sh on Posix shell. Otherwise, we use gcc to compile pnut.
safe=0                  # Whether to use safe mode when compiling pnut (adds checks at run time)
fast=0                  # Whether to use fast mode when compiling pnut (faster shell code)

while [ $# -gt 0 ]; do
  case $1 in
    --gcc)     use_gcc=1;                               shift 1 ;;
    --shell)   shell="$2";                              shift 2 ;;
    --fast)    PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS_FAST"; shift 1 ;;
    --safe)    safe=1;                                  shift 1 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

# Check that --gcc and --shell are not used together
if [ $use_gcc -eq 1 ] && [ -n "$shell" ]; then
  echo "Cannot use --gcc and --shell at the same time"
  exit 1
fi

if [ $safe -eq 1 ]; then
  # Safe mode adds checks at run time to make sure the AST is well constructed
  # and no out-of-bounds access are done. This is mainly useful for debugging.
  PNUT_EXE_OPTIONS="$PNUT_EXE_OPTIONS -DSAFE_MODE";
  PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS -DSAFE_MODE";
  PNUT_SH_OPTIONS_FAST="$PNUT_SH_OPTIONS_FAST -DSAFE_MODE";
fi

if [ $fast -eq 1 ]; then
  # Fast mode disables some checks at run time to make the code run faster.
  # This is mainly useful for production code.
  PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS_FAST"
fi

# Utilities

catm() { # Like cat, but takes the output file as the first argument, a path prefix as the second argument and then file paths
  output="$1"; shift
  prefix="$1"; shift
  args=""
  for arg in "$@"; do
    args="$args $prefix/$arg"
  done
  cat $args > "$output"
}

# Step 1: Bootstrap initial version of TCC (tcc-pnut)

make_tcc_pnut() { # $1: C compiler to use, $2: additional options
  CC="$1"
  $CC                                                                          \
    -I portable_libc/include/                                                  \
    -D BOOTSTRAP=1                                                             \
    -D HAVE_LONG_LONG=0                                                        \
    -D TCC_TARGET_${TCC_TARGET_ARCH}=1                                         \
    -D CONFIG_SYSROOT=\"/\"                                                    \
    -D CONFIG_TCC_CRTPREFIX=\"$TEMP_DIR/boot0-lib\"                            \
    -D CONFIG_TCC_ELFINTERP=\"/mes/loader\"                                    \
    -D CONFIG_TCC_SYSINCLUDEPATHS=\"$MES_DIR/include\"                         \
    -D TCC_LIBGCC=\"$TEMP_DIR/boot0-lib/libc.a\"                               \
    -D CONFIG_TCC_LIBTCC1_MES=0                                                \
    -D CONFIG_TCCBOOT=1                                                        \
    -D CONFIG_TCC_STATIC=1                                                     \
    -D CONFIG_USE_LIBGCC=1                                                     \
    -D TCC_VERSION=\"0.9.26\"                                                  \
    -D ONE_SOURCE=1                                                            \
    -D CONFIG_TCCDIR=\"$TEMP_DIR/boot0-lib/tcc\"                               \
    $TCC_DIR/tcc.c                                                             \
    portable_libc/libc.c                                                       \
    $2                                                                         \
    -o $TEMP_DIR/tcc-pnut

  chmod +x $TEMP_DIR/tcc-pnut
}

if [ $use_gcc -eq 0 ]; then

  if [ -n "$shell" ]; then
    # Test with the specified shell
    echo "Rooting bootstrap on $shell"
    # Let's assume we have a premade pnut-sh.sh script
    # In a normal bootstrap, we'd have a precompiled pnut-sh.sh script.
    # Here, let's create it using gcc
    gcc -o $TEMP_DIR/pnut-sh.exe $PNUT_SH_OPTIONS pnut.c
    ./$TEMP_DIR/pnut-sh.exe $PNUT_SH_OPTIONS pnut.c > $TEMP_DIR/pnut-sh.sh
    $shell $TEMP_DIR/pnut-sh.sh $PNUT_EXE_OPTIONS pnut.c > $TEMP_DIR/pnut-exe.sh
    $shell $TEMP_DIR/pnut-exe.sh $PNUT_EXE_OPTIONS -DNO_BUILTIN_LIBC pnut.c > $TEMP_DIR/pnut-exe

  else

    # Bootstrapping on shells can be slow, we can skip this step and use gcc.
    # We know that these 2 methods reach the same executable, so no need to use
    # the slow method when developing.

    gcc pnut.c $PNUT_EXE_OPTIONS -o $TEMP_DIR/pnut-exe-for-pnut-exe 2> /dev/null
    ./$TEMP_DIR/pnut-exe-for-pnut-exe $PNUT_EXE_OPTIONS -DNO_BUILTIN_LIBC pnut.c > $TEMP_DIR/pnut-exe

  fi

  chmod +x $TEMP_DIR/pnut-exe
  sha256sum $TEMP_DIR/pnut-exe

  # We can now compile TCC with pnut-exe, obtained from gcc or pnut-sh.sh.
  make_tcc_pnut "./$TEMP_DIR/pnut-exe" "-D __intptr_t_defined=1"
else
  # To confirm that the result isn't totally wrong, we can check that the
  # executable is the same as the one we would get with gcc.
  make_tcc_pnut "gcc"
fi

# Prepare Mes lib
mkdir -p $MES_DIR/include/arch
cp $MES_DIR/include/linux/${MES_ARCH}/kernel-stat.h $MES_DIR/include/arch/kernel-stat.h
cp $MES_DIR/include/linux/${MES_ARCH}/signal.h $MES_DIR/include/arch/signal.h
cp $MES_DIR/include/linux/${MES_ARCH}/syscall.h $MES_DIR/include/arch/syscall.h

# Create config.h
touch ${MES_DIR}/include/mes/config.h
touch ${TCC_DIR}/config.h

cp mes-config.h $MES_DIR/include/mes/config.h

# Create unified libc file
catm "$TEMP_DIR/unified-libc.c" "$MES_DIR/lib" ctype/isalnum.c ctype/isalpha.c ctype/isascii.c ctype/iscntrl.c ctype/isdigit.c ctype/isgraph.c ctype/islower.c ctype/isnumber.c ctype/isprint.c ctype/ispunct.c ctype/isspace.c ctype/isupper.c ctype/isxdigit.c ctype/tolower.c ctype/toupper.c dirent/closedir.c dirent/__getdirentries.c dirent/opendir.c linux/readdir.c linux/access.c linux/brk.c linux/chdir.c linux/chmod.c linux/clock_gettime.c linux/close.c linux/dup2.c linux/dup.c linux/execve.c linux/fcntl.c linux/fork.c linux/fsync.c linux/fstat.c linux/_getcwd.c linux/getdents.c linux/getegid.c linux/geteuid.c linux/getgid.c linux/getpid.c linux/getppid.c linux/getrusage.c linux/gettimeofday.c linux/getuid.c linux/ioctl.c linux/ioctl3.c linux/kill.c linux/link.c linux/lseek.c linux/lstat.c linux/malloc.c linux/mkdir.c linux/mknod.c linux/nanosleep.c linux/_open3.c linux/pipe.c linux/_read.c linux/readlink.c linux/rename.c linux/rmdir.c linux/setgid.c linux/settimer.c linux/setuid.c linux/signal.c linux/sigprogmask.c linux/symlink.c linux/stat.c linux/time.c linux/unlink.c linux/waitpid.c linux/wait4.c linux/${MES_ARCH}-mes-gcc/_exit.c linux/${MES_ARCH}-mes-gcc/syscall.c linux/${MES_ARCH}-mes-gcc/_write.c math/ceil.c math/fabs.c math/floor.c mes/abtod.c mes/abtol.c mes/__assert_fail.c mes/assert_msg.c mes/__buffered_read.c mes/__init_io.c mes/cast.c mes/dtoab.c mes/eputc.c mes/eputs.c mes/fdgetc.c mes/fdgets.c mes/fdputc.c mes/fdputs.c mes/fdungetc.c mes/globals.c mes/itoa.c mes/ltoab.c mes/ltoa.c mes/__mes_debug.c mes/mes_open.c mes/ntoab.c mes/oputc.c mes/oputs.c mes/search-path.c mes/ultoa.c mes/utoa.c posix/alarm.c posix/buffered-read.c posix/execl.c posix/execlp.c posix/execv.c posix/execvp.c posix/getcwd.c posix/getenv.c posix/isatty.c posix/mktemp.c posix/open.c posix/pathconf.c posix/raise.c posix/sbrk.c posix/setenv.c posix/sleep.c posix/unsetenv.c posix/wait.c posix/write.c stdio/clearerr.c stdio/fclose.c stdio/fdopen.c stdio/feof.c stdio/ferror.c stdio/fflush.c stdio/fgetc.c stdio/fgets.c stdio/fileno.c stdio/fopen.c stdio/fprintf.c stdio/fputc.c stdio/fputs.c stdio/fread.c stdio/freopen.c stdio/fscanf.c stdio/fseek.c stdio/ftell.c stdio/fwrite.c stdio/getc.c stdio/getchar.c stdio/perror.c stdio/printf.c stdio/putc.c stdio/putchar.c stdio/remove.c stdio/snprintf.c stdio/sprintf.c stdio/sscanf.c stdio/ungetc.c stdio/vfprintf.c stdio/vfscanf.c stdio/vprintf.c stdio/vsnprintf.c stdio/vsprintf.c stdio/vsscanf.c stdlib/abort.c stdlib/abs.c stdlib/alloca.c stdlib/atexit.c stdlib/atof.c stdlib/atoi.c stdlib/atol.c stdlib/calloc.c stdlib/__exit.c stdlib/exit.c stdlib/free.c stdlib/mbstowcs.c stdlib/puts.c stdlib/qsort.c stdlib/realloc.c stdlib/strtod.c stdlib/strtof.c stdlib/strtol.c stdlib/strtold.c stdlib/strtoll.c stdlib/strtoul.c stdlib/strtoull.c string/bcmp.c string/bcopy.c string/bzero.c string/index.c string/memchr.c string/memcmp.c string/memcpy.c string/memmem.c string/memmove.c string/memset.c string/rindex.c string/strcat.c string/strchr.c string/strcmp.c string/strcpy.c string/strcspn.c string/strdup.c string/strerror.c string/strlen.c string/strlwr.c string/strncat.c string/strncmp.c string/strncpy.c string/strpbrk.c string/strrchr.c string/strspn.c string/strstr.c string/strupr.c stub/atan2.c stub/bsearch.c stub/chown.c stub/__cleanup.c stub/cos.c stub/ctime.c stub/exp.c stub/fpurge.c stub/freadahead.c stub/frexp.c stub/getgrgid.c stub/getgrnam.c stub/getlogin.c stub/getpgid.c stub/getpgrp.c stub/getpwnam.c stub/getpwuid.c stub/gmtime.c stub/ldexp.c stub/localtime.c stub/log.c stub/mktime.c stub/modf.c stub/mprotect.c stub/pclose.c stub/popen.c stub/pow.c stub/putenv.c stub/rand.c stub/realpath.c stub/rewind.c stub/setbuf.c stub/setgrent.c stub/setlocale.c stub/setvbuf.c stub/sigaction.c stub/sigaddset.c stub/sigblock.c stub/sigdelset.c stub/sigemptyset.c stub/sigsetmask.c stub/sin.c stub/sys_siglist.c stub/system.c stub/sqrt.c stub/strftime.c stub/times.c stub/ttyname.c stub/umask.c stub/utime.c ${MES_ARCH}-mes-gcc/setjmp.c

go() { # $1: name of bootstrap comp, $2: name of new compiler, $3: lib path (= $2 if empty)
  CC="$1"
  NEW_CC="$2" # Suffix of new compiler: "boot0", "boot1", "boot2", ...
  if [ $# -lt 3 ]; then
    LIB_PATH="$TEMP_DIR/$NEW_CC-lib"
  else
    LIB_PATH="$TEMP_DIR/$3-lib"
  fi

  mkdir -p "$LIB_PATH"
  mkdir -p "$LIB_PATH/tcc"

  for file in "crt1" "crtn" "crti"; do
    $CC -c                                                                     \
      -D HAVE_CONFIG_H=1                                                       \
      -I $MES_DIR/include                                                      \
      -I $MES_DIR/include/linux/${MES_ARCH}                                    \
      $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/$file.c                           \
      -o $LIB_PATH/$file.o
  done

  # libc+gcc.a
  $CC -c                                                                       \
    -D HAVE_CONFIG_H=1                                                         \
    -I $MES_DIR/include                                                        \
    -I $MES_DIR/include/linux/${MES_ARCH}                                      \
    $TEMP_DIR/unified-libc.c                                                   \
    -o $LIB_PATH/unified-libc.o
  $CC -ar cr $LIB_PATH/libc.a $LIB_PATH/unified-libc.o

  # libtcc1.a
  $CC -c -o $LIB_PATH/libtcc1.o kit/libtcc1.c
  $CC -ar cr $LIB_PATH/tcc/libtcc1.a $LIB_PATH/libtcc1.o

  # We can now compile tcc-$NEW_CC
  $CC \
      -g \
      -v \
      -static \
      -o $TEMP_DIR/tcc-$NEW_CC \
      -D BOOTSTRAP=1 \
      -D HAVE_FLOAT=1 \
      -D HAVE_BITFIELD=1 \
      -D HAVE_LONG_LONG=1 \
      -D HAVE_SETJMP=1 \
      -I $MES_DIR/include \
      -D TCC_TARGET_${TCC_TARGET_ARCH}=1 \
      -D CONFIG_TCCDIR=\"$LIB_PATH/tcc\" \
      -D CONFIG_TCC_CRTPREFIX=\"$LIB_PATH\" \
      -D CONFIG_TCC_LIBPATHS=\"$LIB_PATH:$LIB_PATH/tcc\" \
      -D CONFIG_TCC_SYSINCLUDEPATHS=\"$MES_DIR/include\" \
      -D TCC_LIBGCC=\"$LIB_PATH/libc.a\" \
      -D TCC_LIBTCC1=\"libtcc1.a\" \
      -D CONFIG_TCC_ELFINTERP=\"/mes/loader\" \
      -D CONFIG_TCCBOOT=1 \
      -D CONFIG_TCC_STATIC=1 \
      -D CONFIG_USE_LIBGCC=1 \
      -D TCC_VERSION=\"0.9.26\" \
      -D ONE_SOURCE=1 \
      -L $LIB_PATH \
      $TCC_DIR/tcc.c

  sha256sum $LIB_PATH/crt1.o $LIB_PATH/crtn.o $LIB_PATH/crti.o
}

go "$TEMP_DIR/tcc-pnut" "boot0"
go "$TEMP_DIR/tcc-boot0" "boot1"
go "$TEMP_DIR/tcc-boot1" "boot2"
# Make sure we've reached a fixed point
go "$TEMP_DIR/tcc-boot2" "boot3" "boot2"

# Confirm with hashes that we're at a fixed point
sha256sum $TEMP_DIR/boot0-lib/crt1.o $TEMP_DIR/boot1-lib/crt1.o $TEMP_DIR/boot2-lib/crt1.o
sha256sum $TEMP_DIR/boot0-lib/tcc/libtcc1.a $TEMP_DIR/boot0-lib/libc.a
sha256sum $TEMP_DIR/boot1-lib/tcc/libtcc1.a $TEMP_DIR/boot1-lib/libc.a
sha256sum $TEMP_DIR/boot2-lib/tcc/libtcc1.a $TEMP_DIR/boot2-lib/libc.a
sha256sum $TEMP_DIR/tcc-boot0 $TEMP_DIR/tcc-boot1 $TEMP_DIR/tcc-boot2 $TEMP_DIR/tcc-boot3
