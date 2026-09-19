#! /bin/sh
# list-bootstrap-files.sh: List the files that are included in the jammed.sh
#                          archive, along with their extraction paths.
# Example usage:
#   ./kit/list-bootstrap-files.sh
#
# Options:
#   --include-utils: Include some debug scripts in the jammed archive.
#   --tcc-version <version>: Specify the TCC version to use (default: 0.9.27)
#   --mes-libc: Use mes libc instead of pnut libc (default: pnut libc)
#   --extract-archives: Extract .tar.gz files instead of passing them to jam.sh

set -e -u

error() {
  printf "Error: %s\n" "$1" >&2
  exit 1
}

readonly TEMP_DIR="build/kit"
mkdir -p "$TEMP_DIR"

: ${PNUT_OPTIONS:=} # Default to empty options
INCLUDE_UTILS=0
MES_LIBC_VERSION="0.27"
TCC_VERSION="0.9.27"
EXTRACT_ARCHIVES=0
USE_MES_LIBC=0
TCC_OPTIONS="-DONE_SOURCE -DTCC_TARGET_I386"

while [ $# -gt 0 ]; do
  case $1 in
    --include-utils)    INCLUDE_UTILS=1;          shift 1 ;;
    --extract-archives) EXTRACT_ARCHIVES=1;       shift 1 ;;
    --mes-libc)         USE_MES_LIBC=1;           shift 1 ;;
    --tcc-version)
      if [ $# -lt 2 ]; then error "--tcc-version: missing argument"; fi
      TCC_VERSION="$2";                           shift 2 ;;
    *)               error "Unknown option: $1"           ;;
  esac
done

readonly MES_ARCHIVE="kit/mes-0.27.tar.gz"
readonly MES_ARCHIVE_DIR="$TEMP_DIR/mes-0.27"
readonly MES_ARCH=x86

if [ $TCC_VERSION = 0.9.27 ]; then
  readonly TCC_ARCHIVE=kit/tcc-0.9.27.tar.gz
  readonly TCC_DIR=tcc-0.9.27
elif [ $TCC_VERSION = 0.9.26 ]; then
  readonly TCC_ARCHIVE=kit/tcc-0.9.26.tar.gz
  readonly TCC_DIR=tcc-0.9.26-1147-gee75a10c
else
  error "Unsupported TCC version: $TCC_VERSION"
fi

# Compute the list of files needed to compile program, given compilation options
program_dependencies() { # $1: file, $2: compile options
  file="$1"
  comp_options="$2"
  deps=$(gcc -MM "$file" $comp_options)
  echo $(echo "$deps" | tr ':' '\n' | tr '\\' ' ' | sed '1d')
}

strip_path_prefix() { # $1: prefix to strip, $2: path
  prefix=$1
  path=$2

  if [ "${path#"$prefix"}" != "$path" ]; then
    echo "${path#"$prefix"}"
  else
    error "Path '$path' does not start with prefix '$prefix'"
  fi
}

FILES="" # Paths of all files added to jam archive

# Add a file or directory to the list of files to be jammed, with an optional
# destination path. When encountering a directory, it will recursively add all
# files in that directory, adding to the destination path the relative path of
# the file within the directory.
add_file_recursive() { # $1: file or directory, $2: extraction path (optional)
  file=$1

  if [ -d "$file" ]; then
    for sub_file in "$file"/*; do
      if [ -z "${2:-}" ]; then
        # if no extraction path is set, use file path as the destination
        add_file_recursive "$sub_file" "$sub_file"
      else
        # otherwise simply extend the destination path with the sub_file name
        add_file_recursive "$sub_file" "${2}/$(basename $sub_file)"
      fi
    done
  elif [ -f "$file" ]; then
    # Just a simple file, add it.
    if [ -n "${2:-}" ]; then
      FILES="$FILES $file:$2"
    else
      FILES="$FILES $file"
    fi
  else
    printf "Error: '$file' not a file or directory.\n" >&2
    exit 1
  fi
}

# Prepare pnut-sh.sh
PNUT_SH_OPTIONS="$PNUT_OPTIONS -Dtarget_sh -DPNUT_BOOTSTRAP"
gcc -o "$TEMP_DIR/pnut-sh" $PNUT_SH_OPTIONS pnut.c
./$TEMP_DIR/pnut-sh $PNUT_SH_OPTIONS pnut.c > "$TEMP_DIR/pnut-sh.sh"

# Prepare bintools
make kit/bintools.c > /dev/null

# Bootstrap seed
add_file_recursive "$TEMP_DIR/pnut-sh.sh" "pnut-sh.sh"

# Add the dependencies of pnut.c for the complete bootstrap
for dep in $(program_dependencies 'pnut.c' '-Dtarget_i386_linux -DBOOTSTRAP_TCC'); do
  add_file_recursive "$dep"
done

# Emulated 64-bit arithmetic for TCC bootstrapping
add_file_recursive "arith64.c" "arith64.c"

# Bintools source files...
add_file_recursive "kit/bintools.c" "bintools.c"
add_file_recursive "kit/bintools-libc.c" "bintools-libc.c"

# ...and a flat copy of the portable libc to bootstrap it
add_file_recursive "portable_libc/include/fcntl.h"      "fcntl.h"
add_file_recursive "portable_libc/include/math.h"       "math.h"
add_file_recursive "portable_libc/include/pnut_lib.h"   "pnut_lib.h"
add_file_recursive "portable_libc/include/setjmp.h"     "setjmp.h"
add_file_recursive "portable_libc/include/stdio.h"      "stdio.h"
add_file_recursive "portable_libc/include/stdlib.h"     "stdlib.h"
add_file_recursive "portable_libc/include/string.h"     "string.h"
add_file_recursive "portable_libc/include/sys/stat.h"   "stat.h"
add_file_recursive "portable_libc/include/sys/types.h"  "types.h"
add_file_recursive "portable_libc/include/unistd.h"     "unistd.h"
add_file_recursive "portable_libc/include/stdarg.h"     "stdarg.h"
add_file_recursive "portable_libc/src/math.c"           "math.c"
add_file_recursive "portable_libc/src/pnut_lib.c"       "pnut_lib.c"
add_file_recursive "portable_libc/src/setjmp.c"         "setjmp.c"
add_file_recursive "portable_libc/src/stdio.c"          "stdio.c"
add_file_recursive "portable_libc/src/stdlib.c"         "stdlib.c"
add_file_recursive "portable_libc/src/string.c"         "string.c"

# Include full pnut libc, wth proper path
add_file_recursive "portable_libc/include"
add_file_recursive "portable_libc/src"
add_file_recursive "portable_libc/libc.c"

# Bootstrap script (starts from pnut-sh.sh, ends at TCC)
add_file_recursive "kit/bootstrap.sh" "bootstrap.sh"

# TCC patches and auxiliary files
add_file_recursive "kit/tcc-patches/$TCC_VERSION" "tcc-patches"
add_file_recursive "kit/libtcc1.c"
add_file_recursive "kit/config.h"
add_file_recursive "kit/mes-config.h"

# cat.sh is always used by the bootstrap.sh script, so it is always included. The other utils are optional.
add_file_recursive "utils/cat.sh"                         "cat.sh"
if [ $INCLUDE_UTILS -eq 1 ]; then
  add_file_recursive "utils/jam.sh"                         "jam.sh"
  add_file_recursive "utils/ls.sh"                          "ls.sh"
  add_file_recursive "utils/touch.sh"                       "touch.sh"
  add_file_recursive "utils/wc.sh"                          "wc.sh"
fi

if [ $EXTRACT_ARCHIVES -eq 0 ]; then
  add_file_recursive "$TCC_ARCHIVE" "tcc-${TCC_VERSION}.tar.gz"
else
  tar -xzf "$TCC_ARCHIVE" -C "$TEMP_DIR"
  touch "$TEMP_DIR/$TCC_DIR/config.h"
  deps=$(program_dependencies "$TEMP_DIR/$TCC_DIR/tcc.c" "$TCC_OPTIONS")
  for dep in $deps; do
    add_file_recursive "$dep" "$(strip_path_prefix "$TEMP_DIR/" "$dep")"
  done
fi

if [ $USE_MES_LIBC -eq 1 ]; then
  if [ $EXTRACT_ARCHIVES -eq 1 ]; then
    tar -xzf "$MES_ARCHIVE" -C "$TEMP_DIR"
    add_file_recursive "$MES_ARCHIVE_DIR/include" "mes-${MES_LIBC_VERSION}/include"
    for sublib in ctype dirent linux $MES_ARCH-mes-gcc linux/$MES_ARCH-mes-gcc math mes posix stdio stdlib string stub; do
      add_file_recursive "$MES_ARCHIVE_DIR/lib/$sublib" "mes-${MES_LIBC_VERSION}/lib/$sublib"
    done
  else
    add_file_recursive "$MES_ARCHIVE" "mes-${MES_LIBC_VERSION}.tar.gz"
  fi
fi

for ARG in $FILES; do
  printf "%s\n" "$ARG"
done
