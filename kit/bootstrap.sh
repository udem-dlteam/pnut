#! /bin/sh
#
# Script to bootstrap TCC after jammed.sh was extracted.
# It prepares the environment and builds the necessary tools, before building TCC.
# This script assumes it runs in an environment where the jammed.sh archive has
# been extracted, and that the necessary files are present.

set -e -u -x

error() {
  printf "Error: %s\n" "$1" >&2
  exit 1
}

log() {
  printf "%s\n" "$1" >&2
}

: ${PNUT_OPTIONS:=}         # Default to empty options
: ${INSTALL_EXECS:=0}       # Default to not installing
: ${MES_LIBC_VERSION:=0.27} # Default mes libc version
: ${USE_GCC:=0}             # Default to not using gcc for bootstrapping TCC

# MUST BE KEPT IN SYNC WITH kit/setup-rootfs.sh
PNUT_ARCH=i386_linux
PNUT_EXE_OPTIONS="$PNUT_OPTIONS -Dtarget_$PNUT_ARCH -DONE_PASS_GENERATOR"
PNUT_EXE_TCC_OPTIONS="$PNUT_EXE_OPTIONS -DSUPPORT_EMULATED_INT64 -DUNDEFINED_LABELS_ARE_RUNTIME_ERRORS -DENABLE_PNUT_INLINE_INTERRUPT -DNO_BUILTIN_LIBC"

# 1. Unpack jammed.sh (already done)
# ./jammed.sh

# 2. Copy jammed.sh if jammed-no-exec.sh doesn't exist
if [ ! -e "jammed-no-exec.sh" ]; then
  $BOOTSTRAP_SHELL cat.sh jammed.sh > jammed-no-exec.sh
fi

# 3. Bootstrap pnut-exe (if necessary)
if [ ! -e "pnut-exe" ]; then
  # 3a. Bootstrap pnut-exe
  log "Bootstrapping minimal pnut-exe from pnut-sh.sh"
  $BOOTSTRAP_SHELL pnut-sh.sh pnut.c $PNUT_EXE_OPTIONS -DPNUT_BOOTSTRAP > pnut-exe.sh
  # 3b. Make executable version of pnut-exe. Overwrite jammed.sh to reuse its execute bit.
  log "Making executable pnut-exe (minimal)"
  $BOOTSTRAP_SHELL pnut-exe.sh pnut.c $PNUT_EXE_OPTIONS -DPNUT_BOOTSTRAP -o jammed.sh
  # Compile complete variant of pnut-exe with minimal pnut-exe (named jammed.sh)
  log "Making executable pnut-exe (complete)"
  ./jammed.sh pnut.c $PNUT_EXE_TCC_OPTIONS -o pnut-exe
else
  log "pnut-exe already exists, skipping pnut-exe bootstrap"
fi

# 4. Compile bintools with pnut-exe (named jammed.sh)
if [ ! -e "bintools" ]; then
  log "Compiling bintools from pnut-exe"
  ./pnut-exe -D FLAT_INCLUDES -I "./" -rt arith64.c bintools.c bintools-libc.c -o bintools
fi

# 5. Install bintools and pnut-exe
if [ $INSTALL_EXECS -eq 1 ]; then
  log "Installing bintools and pnut-exe"
  ./bintools mkdir -p /usr/bin
  for tool in cp chmod mkdir sha256sum simple-patch ungz untar; do
    ./bintools cp ./bintools /usr/bin/$tool
    ./bintools chmod 755 /usr/bin/$tool
  done
  ./bintools cp ./pnut-exe /usr/bin/pnut-exe
  ./bintools chmod 755 /usr/bin/pnut-exe
else
  log "Skipping installation of bintools and pnut-exe as requested"
fi

# 6. Extract the rest of the files (now that mkdir is available)
$BOOTSTRAP_SHELL ./jammed-no-exec.sh --force-no-exec

################################################################################
################################ TCC bootstrap #################################
################################################################################

TCC_TARGET_ARCH=I386

TCC_0_9_26_PATCHES="
tccpp.c  array_sizeof
libtcc.c error_set_jmp_enabled
tccgen.c fix_stack_64_bit_operands_on_32_bit
tccgen.c float_negation
tccpp.c  long_long_parser
libtcc.c sscanf_TCC_VERSION
tcc.h    undefine_TCC_IS_NATIVE
"

TCC_0_9_27_PATCHES="
tccpp.c   array_sizeof
tcc.h     attribute
tcc.h     bitfields
tccgen.c  float_negation
tccgen.c  float_zero_division_check
tccgen.c  long_double_codegen
tccpp.c   scientific-notation-parser
libtcc.c  sscanf_TCC_VERSION
"

PATCHES_DIR="tcc-patches"
INCLUDE_PATH="portable_libc/include" # pnut libc

TEMP_DIR="build"
if [ ! -d "$TEMP_DIR" ]; then ./bintools mkdir -p "$TEMP_DIR"; fi

MES_ARCH=x86
MES_ARCHIVE=mes-${MES_LIBC_VERSION}.tar.gz
MES_DIR=mes-${MES_LIBC_VERSION}

# 7. Unpack tcc and mes libc if in .tar.gz
#
# Because we want to support multiple TCC versions, we recognize from the file
# name which version of TCC was included in the environment. Detecting which
# version saves us from repeatedly passing the TCC version as an argument to the
# bootstrap scripts.

if [ -e "kit/tcc-0.9.27.tar.gz" ] || [ -e "tcc-0.9.27" ]; then
  TCC_VERSION=0.9.27
  TCC_ARCHIVE=kit/tcc-0.9.27.tar.gz
  TCC_DIR=tcc-0.9.27
  TCC_PATCHES="$TCC_0_9_27_PATCHES"
elif [ -e "kit/tcc-0.9.26.tar.gz" ] || [ -e "tcc-0.9.26-1147-gee75a10c" ]; then
  TCC_VERSION=0.9.26
  TCC_ARCHIVE=kit/tcc-0.9.26.tar.gz
  TCC_DIR=tcc-0.9.26-1147-gee75a10c
  TCC_PATCHES="$TCC_0_9_26_PATCHES"
else
  error "Missing tcc archive or unrecognized version. Supported versions are 0.9.26 and 0.9.27."
fi

if [ -e "$TCC_ARCHIVE" ]; then
  ./bintools ungz --file "$TCC_ARCHIVE" --output "tcc-${TCC_VERSION}.tar"
  ./bintools untar "tcc-${TCC_VERSION}.tar"
fi

if [ -e "$MES_ARCHIVE" ]; then
  ./bintools ungz --file "$MES_ARCHIVE" --output "mes-${MES_LIBC_VERSION}.tar"
  ./bintools untar "mes-${MES_LIBC_VERSION}.tar"
fi

# 8. Patch TCC and prepare libc

apply_tcc_patches() { # $1..: patches list
  while [ $# -gt 0 ]; do
    target_file="$TCC_DIR/$1"
    patch_file_base="$PATCHES_DIR/$2"

    ./bintools simple-patch "$target_file" "$patch_file_base.before" "$patch_file_base.after" \
      || { error "Failed to apply patch $patch_file_base to $target_file"; }
    shift 2
  done
}

# No need to revert patches, because the ones that are applied all keep the
# existing code behind an #ifdef PNUT_CC directive.
# revert_tcc_patches() { # $1..: patches list
#   while [ $# -gt 0 ]; do
#     target_file="$TCC_DIR/$1"
#     patch_file_base="$PATCHES_DIR/$2"

#     ./bintools simple-patch "$target_file" "$patch_file_base.after" "$patch_file_base.before" \
#       || { error "Failed to revert patch $patch_file_base on $target_file"; }
#     shift 2
#   done
# }

./bintools cp kit/config.h "$TCC_DIR/config.h"

apply_tcc_patches $TCC_PATCHES

# If using mes libc, prepare the include files and create a unified libc file
if [ -e "mes-0.27" ]; then
  INCLUDE_PATH="$MES_DIR/include"

  # Prepare Mes lib
  ./bintools mkdir -p $MES_DIR/include/arch
  ./bintools cp $MES_DIR/include/linux/${MES_ARCH}/kernel-stat.h $MES_DIR/include/arch/kernel-stat.h
  ./bintools cp $MES_DIR/include/linux/${MES_ARCH}/signal.h $MES_DIR/include/arch/signal.h
  ./bintools cp $MES_DIR/include/linux/${MES_ARCH}/syscall.h $MES_DIR/include/arch/syscall.h

  # Create empty config.h
  printf "\n" > ${MES_DIR}/include/mes/config.h

  ./bintools cp kit/mes-config.h $MES_DIR/include/mes/config.h

  # Create unified libc file
  MES_LIBC_FILES="ctype/isalnum.c ctype/isalpha.c ctype/isascii.c ctype/iscntrl.c ctype/isdigit.c ctype/isgraph.c ctype/islower.c ctype/isnumber.c ctype/isprint.c ctype/ispunct.c ctype/isspace.c ctype/isupper.c ctype/isxdigit.c ctype/tolower.c ctype/toupper.c dirent/closedir.c dirent/__getdirentries.c dirent/opendir.c linux/readdir.c linux/access.c linux/brk.c linux/chdir.c linux/chmod.c linux/clock_gettime.c linux/close.c linux/dup2.c linux/dup.c linux/execve.c linux/fcntl.c linux/fork.c linux/fsync.c linux/fstat.c linux/_getcwd.c linux/getdents.c linux/getegid.c linux/geteuid.c linux/getgid.c linux/getpid.c linux/getppid.c linux/getrusage.c linux/gettimeofday.c linux/getuid.c linux/ioctl.c linux/ioctl3.c linux/kill.c linux/link.c linux/lseek.c linux/lstat.c linux/malloc.c linux/mkdir.c linux/mknod.c linux/nanosleep.c linux/_open3.c linux/pipe.c linux/_read.c linux/readlink.c linux/rename.c linux/rmdir.c linux/setgid.c linux/settimer.c linux/setuid.c linux/signal.c linux/sigprogmask.c linux/symlink.c linux/stat.c linux/time.c linux/unlink.c linux/waitpid.c linux/wait4.c linux/${MES_ARCH}-mes-gcc/_exit.c linux/${MES_ARCH}-mes-gcc/syscall.c linux/${MES_ARCH}-mes-gcc/_write.c math/ceil.c math/fabs.c math/floor.c mes/abtod.c mes/abtol.c mes/__assert_fail.c mes/assert_msg.c mes/__buffered_read.c mes/__init_io.c mes/cast.c mes/dtoab.c mes/eputc.c mes/eputs.c mes/fdgetc.c mes/fdgets.c mes/fdputc.c mes/fdputs.c mes/fdungetc.c mes/globals.c mes/itoa.c mes/ltoab.c mes/ltoa.c mes/__mes_debug.c mes/mes_open.c mes/ntoab.c mes/oputc.c mes/oputs.c mes/search-path.c mes/ultoa.c mes/utoa.c posix/alarm.c posix/buffered-read.c posix/execl.c posix/execlp.c posix/execv.c posix/execvp.c posix/getcwd.c posix/getenv.c posix/isatty.c posix/mktemp.c posix/open.c posix/pathconf.c posix/raise.c posix/sbrk.c posix/setenv.c posix/sleep.c posix/unsetenv.c posix/wait.c posix/write.c stdio/clearerr.c stdio/fclose.c stdio/fdopen.c stdio/feof.c stdio/ferror.c stdio/fflush.c stdio/fgetc.c stdio/fgets.c stdio/fileno.c stdio/fopen.c stdio/fprintf.c stdio/fputc.c stdio/fputs.c stdio/fread.c stdio/freopen.c stdio/fscanf.c stdio/fseek.c stdio/ftell.c stdio/fwrite.c stdio/getc.c stdio/getchar.c stdio/perror.c stdio/printf.c stdio/putc.c stdio/putchar.c stdio/remove.c stdio/snprintf.c stdio/sprintf.c stdio/sscanf.c stdio/ungetc.c stdio/vfprintf.c stdio/vfscanf.c stdio/vprintf.c stdio/vsnprintf.c stdio/vsprintf.c stdio/vsscanf.c stdlib/abort.c stdlib/abs.c stdlib/alloca.c stdlib/atexit.c stdlib/atof.c stdlib/atoi.c stdlib/atol.c stdlib/calloc.c stdlib/__exit.c stdlib/exit.c stdlib/free.c stdlib/mbstowcs.c stdlib/puts.c stdlib/qsort.c stdlib/realloc.c stdlib/strtod.c stdlib/strtof.c stdlib/strtol.c stdlib/strtold.c stdlib/strtoll.c stdlib/strtoul.c stdlib/strtoull.c string/bcmp.c string/bcopy.c string/bzero.c string/index.c string/memchr.c string/memcmp.c string/memcpy.c string/memmem.c string/memmove.c string/memset.c string/rindex.c string/strcat.c string/strchr.c string/strcmp.c string/strcpy.c string/strcspn.c string/strdup.c string/strerror.c string/strlen.c string/strlwr.c string/strncat.c string/strncmp.c string/strncpy.c string/strpbrk.c string/strrchr.c string/strspn.c string/strstr.c string/strupr.c stub/atan2.c stub/bsearch.c stub/chown.c stub/__cleanup.c stub/cos.c stub/ctime.c stub/exp.c stub/fpurge.c stub/freadahead.c stub/frexp.c stub/getgrgid.c stub/getgrnam.c stub/getlogin.c stub/getpgid.c stub/getpgrp.c stub/getpwnam.c stub/getpwuid.c stub/gmtime.c stub/ldexp.c stub/localtime.c stub/log.c stub/mktime.c stub/modf.c stub/mprotect.c stub/pclose.c stub/popen.c stub/pow.c stub/putenv.c stub/rand.c stub/realpath.c stub/rewind.c stub/setbuf.c stub/setgrent.c stub/setlocale.c stub/setvbuf.c stub/sigaction.c stub/sigaddset.c stub/sigblock.c stub/sigdelset.c stub/sigemptyset.c stub/sigsetmask.c stub/sin.c stub/sys_siglist.c stub/system.c stub/sqrt.c stub/strftime.c stub/times.c stub/ttyname.c stub/umask.c stub/utime.c ${MES_ARCH}-mes-gcc/setjmp.c"

  # Cat all the mes libc files into a single file for TCC to compile
  # Note that printf is used to prefix the file names with the mes directory
  ./bintools cat $(printf "$MES_DIR/lib/%s " $MES_LIBC_FILES) > "$TEMP_DIR/unified-libc.c"
else
  # No mes libc, use portable libc
  INCLUDE_PATH="portable_libc/include"
fi

# 9: Bootstrap initial version of TCC (tcc-pnut)

make_tcc_bootstrap() { # $1: C compiler to use, $2: additional options
  CC="$1"
  case "$CC" in
    gcc*|clang*)
      EXTRA_OPTS="              \
        -D HAVE_FLOAT=1         \
        -D HAVE_BITFIELD=1      \
        -D HAVE_LONG_LONG=1     \
        -D HAVE_SETJMP=1"
      ;;
    *pnut-exe*)
      EXTRA_OPTS="              \
      -rt arith64.c             \
      -I portable_libc/include/ \
      portable_libc/libc.c      \
      "
      ;;
    *)
      echo "Unknown C compiler: $CC" >&2; exit 1
      ;;
  esac
  $CC                                                                          \
    -D BOOTSTRAP=1                                                             \
    -D PNUT_CC=1                                                               \
    -D HAVE_LONG_LONG=1                                                        \
    -D TCC_TARGET_${TCC_TARGET_ARCH}=1                                         \
    -D CONFIG_SYSROOT=\"/\"                                                    \
    -D CONFIG_TCC_CRTPREFIX=\"$TEMP_DIR/boot0-lib\"                            \
    -D CONFIG_TCC_ELFINTERP=\"/mes/loader\"                                    \
    -D CONFIG_TCC_SYSINCLUDEPATHS=\"$INCLUDE_PATH\"                            \
    -D TCC_LIBGCC=\"$TEMP_DIR/boot0-lib/libc.a\"                               \
    -D CONFIG_TCC_LIBTCC1_MES=0                                                \
    -D CONFIG_TCCBOOT=1                                                        \
    -D CONFIG_TCC_STATIC=1                                                     \
    -D CONFIG_USE_LIBGCC=1                                                     \
    -D TCC_VERSION=\"$TCC_VERSION\"                                            \
    -D ONE_SOURCE=1                                                            \
    -D CONFIG_TCCDIR=\"$TEMP_DIR/boot0-lib/tcc\"                               \
    $TCC_DIR/tcc.c                                                             \
    $EXTRA_OPTS                                                                \
    $2                                                                         \
    -o $TEMP_DIR/tcc-pnut

  ./bintools chmod 755 $TEMP_DIR/tcc-pnut
}

if [ $USE_GCC -eq 0 ]; then
  # Recompile pnut in safe mode and with the portable libc to have decent error messages
  ./pnut-exe                        \
    pnut.c                          \
    $PNUT_EXE_TCC_OPTIONS           \
    -DSAFE_MODE                     \
    -rt arith64.c                   \
    -I portable_libc/include/       \
    portable_libc/libc.c            \
    -o "$TEMP_DIR/pnut-exe-for-tcc"

  make_tcc_bootstrap "$TEMP_DIR/pnut-exe-for-tcc" "-D __intptr_t_defined=1"
else
  # To confirm that the result isn't totally wrong, we can check that the
  # executable is the same as the one we would get with gcc. Assuming that the
  # system has a working gcc.
  make_tcc_bootstrap "gcc -m32 -std=c99" ""
fi

# No need to revert patches, because the ones that are applied all keep the
# existing code behind an #ifdef PNUT_CC directive.
# revert_tcc_patches $TCC_PATCHES

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

  # If building with the mes libc, we need to build the crt1.o, crtn.o, crti.o
  # files and the unified-libc.o file.
  if [ -e "mes-0.27" ]; then
    for file in "crt1" "crtn" "crti"; do
      $CC -c                                                                   \
        -D HAVE_CONFIG_H=1                                                     \
        -I $MES_DIR/include                                                    \
        -I $MES_DIR/include/linux/${MES_ARCH}                                  \
        $MES_DIR/lib/linux/${MES_ARCH}-mes-gcc/$file.c                         \
        -o $LIB_PATH/$file.o
    done
    # libc+gcc.a
    $CC -c                                                                     \
      -D HAVE_CONFIG_H=1                                                       \
      -I $MES_DIR/include                                                      \
      -I $MES_DIR/include/linux/${MES_ARCH}                                    \
      $TEMP_DIR/unified-libc.c                                                 \
      -o $LIB_PATH/unified-libc.o

    $CC -ar cr $LIB_PATH/libc.a $LIB_PATH/unified-libc.o
  else
    # With the pnut libc, we can just compile the crt1.o file and create an
    # empty crtn.o and crti.o files.
    # The libc.a file is created from the portable_libc/libc.c file.
    $CC -c portable_libc/src/crt1.c -o "$LIB_PATH/crt1.o"
    printf "" > "$LIB_PATH/crtn.o" # Empty file
    printf "" > "$LIB_PATH/crti.o" # Empty file

    $CC -c -D ADD_LIBC_STUB -I portable_libc/include -o "$LIB_PATH/libc.o" portable_libc/libc.c
    $CC -ar cr "$LIB_PATH/libc.a" "$LIB_PATH/libc.o"
  fi

  # libtcc1.a
  $CC -c -o "$LIB_PATH/libtcc1.o" kit/libtcc1.c
  $CC -ar cr "$LIB_PATH/tcc/libtcc1.a" "$LIB_PATH/libtcc1.o"

  # We can now compile tcc-$NEW_CC
  $CC \
      -v \
      -static \
      -o $TEMP_DIR/tcc-$NEW_CC \
      -D BOOTSTRAP=1 \
      -D __SIZEOF_LONG_LONG__=8 \
      -D HAVE_FLOAT=1 \
      -D HAVE_BITFIELD=1 \
      -D HAVE_LONG_LONG=1 \
      -D HAVE_SETJMP=1 \
      -I $INCLUDE_PATH \
      -D TCC_TARGET_${TCC_TARGET_ARCH}=1 \
      -D CONFIG_TCCDIR=\"$LIB_PATH/tcc\" \
      -D CONFIG_TCC_CRTPREFIX=\"$LIB_PATH\" \
      -D CONFIG_TCC_LIBPATHS=\"$LIB_PATH:$LIB_PATH/tcc\" \
      -D CONFIG_TCC_SYSINCLUDEPATHS=\"$INCLUDE_PATH\" \
      -D TCC_LIBGCC=\"$LIB_PATH/libc.a\" \
      -D TCC_LIBTCC1=\"libtcc1.a\" \
      -D CONFIG_TCC_ELFINTERP=\"/mes/loader\" \
      -D CONFIG_TCCBOOT=1 \
      -D CONFIG_TCC_STATIC=1 \
      -D CONFIG_USE_LIBGCC=1 \
      -D TCC_VERSION=\"${TCC_VERSION}\" \
      -D ONE_SOURCE=1 \
      -L $LIB_PATH \
      $TCC_DIR/tcc.c

  # Create tcc-$NEW_CC.o file to help debugging. Hardcode the compile options
  # with paths to make sure they are the same when using the mes and pnut libc.
  $CC \
      -c \
      -v \
      -static \
      -o $TEMP_DIR/tcc-$NEW_CC.o \
      -D BOOTSTRAP=1 \
      -D __SIZEOF_LONG_LONG__=8 \
      -D HAVE_FLOAT=1 \
      -D HAVE_BITFIELD=1 \
      -D HAVE_LONG_LONG=1 \
      -D HAVE_SETJMP=1 \
      -I $INCLUDE_PATH \
      -D TCC_TARGET_${TCC_TARGET_ARCH}=1 \
      -D CONFIG_TCCDIR=\"$LIB_PATH/tcc\" \
      -D CONFIG_TCC_CRTPREFIX=\"$LIB_PATH\" \
      -D CONFIG_TCC_LIBPATHS=\"$LIB_PATH:$LIB_PATH/tcc\" \
      -D CONFIG_TCC_SYSINCLUDEPATHS=\"SOME_DIRECTORY\" \
      -D TCC_LIBGCC=\"$LIB_PATH/libc.a\" \
      -D TCC_LIBTCC1=\"libtcc1.a\" \
      -D CONFIG_TCC_ELFINTERP=\"/mes/loader\" \
      -D CONFIG_TCCBOOT=1 \
      -D CONFIG_TCC_STATIC=1 \
      -D CONFIG_USE_LIBGCC=1 \
      -D TCC_VERSION=\"${TCC_VERSION}\" \
      -D ONE_SOURCE=1 \
      -L $LIB_PATH \
      $TCC_DIR/tcc.c

  ./bintools sha256sum $LIB_PATH/crt1.o $LIB_PATH/crtn.o $LIB_PATH/crti.o
}

go "$TEMP_DIR/tcc-pnut" "boot0"
go "$TEMP_DIR/tcc-boot0" "boot1"
go "$TEMP_DIR/tcc-boot1" "boot2"
# Make sure we've reached a fixed point
go "$TEMP_DIR/tcc-boot2" "boot3" "boot2"

# Confirm with hashes that we're at a fixed point
./bintools sha256sum $TEMP_DIR/boot0-lib/crt1.o $TEMP_DIR/boot1-lib/crt1.o $TEMP_DIR/boot2-lib/crt1.o
./bintools sha256sum $TEMP_DIR/boot0-lib/tcc/libtcc1.a $TEMP_DIR/boot0-lib/libc.a
./bintools sha256sum $TEMP_DIR/boot1-lib/tcc/libtcc1.a $TEMP_DIR/boot1-lib/libc.a
./bintools sha256sum $TEMP_DIR/boot2-lib/tcc/libtcc1.a $TEMP_DIR/boot2-lib/libc.a
./bintools sha256sum $TEMP_DIR/tcc-boot0.o $TEMP_DIR/tcc-boot1.o $TEMP_DIR/tcc-boot2.o $TEMP_DIR/tcc-boot3.o
./bintools sha256sum $TEMP_DIR/tcc-boot0 $TEMP_DIR/tcc-boot1 $TEMP_DIR/tcc-boot2 $TEMP_DIR/tcc-boot3
