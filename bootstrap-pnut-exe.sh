#! /bin/sh

set -e -u

TEMP_DIR="bootstrap-results"

# PNUT_EXE_OPTIONS="-Dmac_os"
# PNUT_EXE_OPTIONS="-Dx86_64"
PNUT_EXE_OPTIONS="-Di386"
PNUT_SH_OPTIONS="-DRT_NO_INIT_GLOBALS -Dsh"

if [ ! -d "$TEMP_DIR" ]; then mkdir "$TEMP_DIR"; fi

printf_timing() {
  msg=$1
  cmd=$2
  real_time=`/usr/bin/time -p sh -c "$cmd" 2>&1 | grep '^real ' | sed 's/.* //'`
  printf "%ss %s\n" $real_time "$msg"
}

bootstrap_with_gcc() {

  echo "===================== Bootstrap with GCC"

  # Compile pnut x86 using GCC, then compile pnut x86 using pnut x86

  gcc -o $TEMP_DIR/pnut-x86-by-gcc.exe $PNUT_EXE_OPTIONS pnut.c
  # gcc -E -P -DPNUT_CC $PNUT_EXE_OPTIONS pnut.c > "$TEMP_DIR/pnut-after-cpp.c"
  ./$TEMP_DIR/pnut-x86-by-gcc.exe $PNUT_EXE_OPTIONS pnut.c > $TEMP_DIR/pnut-x86-by-pnut-x86-by-gcc.exe

  chmod +x $TEMP_DIR/pnut-x86-by-pnut-x86-by-gcc.exe

  ./$TEMP_DIR/pnut-x86-by-pnut-x86-by-gcc.exe $PNUT_EXE_OPTIONS pnut.c > $TEMP_DIR/pnut-x86-by-pnut-x86-by-pnut-x86-by-gcc.exe

  if [ -s $TEMP_DIR/pnut-x86-by-pnut-x86-by-pnut-x86-by-gcc.exe ] ; then
    if diff $TEMP_DIR/pnut-x86-by-pnut-x86-by-gcc.exe $TEMP_DIR/pnut-x86-by-pnut-x86-by-pnut-x86-by-gcc.exe 2>&1 > /dev/null ; then
      printf "         SUCCESS... %s\n" "pnut-x86-by-pnut-x86-by-gcc.exe == pnut-x86-by-pnut-x86-by-pnut-x86-by-gcc.exe"
    else
      printf "         FAILURE... %s\n" "pnut-x86-by-pnut-x86-by-gcc.exe != pnut-x86-by-pnut-x86-by-pnut-x86-by-gcc.exe"
      exit 1
    fi
  else
    printf "         FAILURE... %s\n" "pnut-x86-by-pnut-x86-by-pnut-x86-by-gcc.exe is empty! (compiler crash?)"
    exit 1
  fi
}

bootstrap_with_shell() {

  echo "===================== Bootstrap with $1"

  # create pnut-sh.sh, the C to shell compiler as a shell script
  gcc -o $TEMP_DIR/pnut-sh-compiled-by-gcc.exe $PNUT_SH_OPTIONS pnut.c
  # gcc -E -P -DPNUT_CC $PNUT_SH_OPTIONS pnut.c > "$TEMP_DIR/pnut-sh-after-cpp.c"
  ./$TEMP_DIR/pnut-sh-compiled-by-gcc.exe $PNUT_SH_OPTIONS pnut.c > $TEMP_DIR/pnut-sh.sh

  # create pnut-i386.sh, the C to i386 machine code compiler as a shell script
  # gcc -E -P -DPNUT_CC $PNUT_EXE_OPTIONS pnut.c > $TEMP_DIR/pnut-i386-after-cpp.c
  printf_timing "pnut-sh.sh compiling pnut.c -> pnut-sh-compiled-by-pnut-sh-sh.sh" \
                "$1 $TEMP_DIR/pnut-sh.sh $PNUT_SH_OPTIONS pnut.c > $TEMP_DIR/pnut-sh-compiled-by-pnut-sh-sh.sh"
  if diff $TEMP_DIR/pnut-sh.sh $TEMP_DIR/pnut-sh-compiled-by-pnut-sh-sh.sh 2>&1 > /dev/null ; then
    printf "         SUCCESS... %s\n" "pnut-sh.sh == pnut-sh-compiled-by-pnut-sh-sh.sh"
  else
    printf "         FAILURE... %s\n" "pnut-sh.sh != pnut-sh-compiled-by-pnut-sh-sh.sh"
    exit 1
  fi
  printf_timing "pnut-sh.sh compiling pnut.c -> pnut-i386-compiled-by-pnut-sh-sh.sh" \
                "$1 $TEMP_DIR/pnut-sh.sh $PNUT_EXE_OPTIONS pnut.c > $TEMP_DIR/pnut-i386-compiled-by-pnut-sh-sh.sh"
  printf_timing "pnut-i386-compiled-by-pnut-sh-sh.sh compiling pnut.c -> pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe" \
                "$1 $TEMP_DIR/pnut-i386-compiled-by-pnut-sh-sh.sh $PNUT_EXE_OPTIONS pnut.c > $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe"

  chmod +x $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe

  printf_timing "pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe compiling pnut.c -> pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe" \
                "./$TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe $PNUT_EXE_OPTIONS pnut.c > $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe"

  chmod +x $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe

  if [ -s $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe ] ; then
    if diff $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe 2>&1 > /dev/null ; then
      printf "         SUCCESS... %s\n" "pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe == pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe"
    else
      printf "         FAILURE... %s\n" "pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe != pnut-sh-compiled-by-pnut-sh-sh.sh"
      exit 1
    fi
  else
    printf "         FAILURE... %s\n" "pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe is empty! (compiler crash?)"
    exit 1
  fi

  if false ; then

    printf "\n"

    ls -l \
    "$TEMP_DIR/pnut-sh.sh" \
    "$TEMP_DIR/pnut-sh-compiled-by-pnut-sh-sh.sh"

    printf "\n"

    sha1sum \
    "$TEMP_DIR/pnut-sh.sh" \
    "$TEMP_DIR/pnut-sh-compiled-by-pnut-sh-sh.sh"

    printf "\n"

    ls -l \
    "$TEMP_DIR/pnut-sh-compiled-by-gcc.exe" \
    "$TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe" \
    "$TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe"

    printf "\n"

    sha1sum \
    "$TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe" \
    "$TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe"

  fi
}

# Parse the arguments
backend="x86_64"  # Default to x86_64 Linux
shell=            # Defined if doing the full bootstrap using pnut.sh on Posix shell. "all" to test with all shells (slow).

while [ $# -gt 0 ]; do
  case $1 in
    --backend) backend="$2"; shift 2 ;;
    --shell) shell="$2"; shift 2 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

case $backend in
  i386 | x86_64 | mac_os)
    PNUT_EXE_OPTIONS="-D$backend" ;;
  *)
    echo "Unknown backend: $backend"
    exit 1
    ;;
esac

if [ -z "$shell" ]; then
  bootstrap_with_gcc
else
  if [ "$shell" = "all" ]; then
    set +e # Don't exit on error because we want to test all shells.
    bootstrap_with_shell "dash"
    bootstrap_with_shell "ksh"
    bootstrap_with_shell "bash"
    bootstrap_with_shell "yash"
    bootstrap_with_shell "mksh"
    bootstrap_with_shell "zsh"
  else
    bootstrap_with_shell "$shell"
  fi
fi
