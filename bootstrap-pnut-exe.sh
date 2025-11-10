#! /bin/sh

set -e -u

TEMP_DIR="build"

: ${PNUT_OPTIONS:=} # Default to empty options

PNUT_EXE_OPTIONS="$PNUT_OPTIONS" # Backend is set by the backend option
PNUT_SH_OPTIONS="$PNUT_OPTIONS -Dsh"
PNUT_SH_OPTIONS_FAST="$PNUT_SH_OPTIONS -DSH_SAVE_VARS_WITH_SET"

add_exe_option() {
  PNUT_EXE_OPTIONS="$PNUT_EXE_OPTIONS -D$1"
}

if [ ! -d "$TEMP_DIR" ]; then mkdir "$TEMP_DIR"; fi

printf_timing() {
  msg=$1
  cmd=$2
  real_time=`env time -p sh -c "$cmd" 2>&1 | grep '^real ' | sed 's/.* //'`
  printf "%s %s\n" $real_time "$msg"
}

bootstrap_with_gcc() {

  echo "===================== Bootstrap with GCC"

  # Compile pnut x86 using GCC, then compile pnut x86 using pnut x86

  gcc -o $TEMP_DIR/pnut-x86-by-gcc.exe $PNUT_EXE_OPTIONS pnut.c
  # gcc -E -P -DPNUT_CC $PNUT_EXE_OPTIONS pnut.c > "$TEMP_DIR/pnut-after-cpp.c"
  ./$TEMP_DIR/pnut-x86-by-gcc.exe $PNUT_EXE_OPTIONS pnut.c -o $TEMP_DIR/pnut-x86-by-pnut-x86-by-gcc.exe || {
    echo "Failed to compile pnut-x86-by-pnut-x86-by-gcc.exe"
    tail -n 20 $TEMP_DIR/pnut-x86-by-pnut-x86-by-gcc.exe
    exit 1
  }

  chmod +x $TEMP_DIR/pnut-x86-by-pnut-x86-by-gcc.exe

  printf_timing "pnut-x86-by-gcc.exe compiling pnut.c -> pnut-x86-by-pnut-x86-by-gcc.exe" \
                "./$TEMP_DIR/pnut-x86-by-pnut-x86-by-gcc.exe $PNUT_EXE_OPTIONS pnut.c -o $TEMP_DIR/pnut-x86-by-pnut-x86-by-pnut-x86-by-gcc.exe"

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

  # create pnut-exe.sh, the C to machine code compiler as a shell script
  if [ $skip_pnut_sh -eq 0 ]; then
    printf_timing "pnut-sh.sh compiling pnut.c -> pnut-sh-compiled-by-pnut-sh-sh.sh" \
                  "$1 $TEMP_DIR/pnut-sh.sh $PNUT_SH_OPTIONS pnut.c > $TEMP_DIR/pnut-sh-compiled-by-pnut-sh-sh.sh"
    if diff $TEMP_DIR/pnut-sh.sh $TEMP_DIR/pnut-sh-compiled-by-pnut-sh-sh.sh 2>&1 > /dev/null ; then
      printf "         SUCCESS... %s\n" "pnut-sh.sh == pnut-sh-compiled-by-pnut-sh-sh.sh"
    else
      printf "         FAILURE... %s\n" "pnut-sh.sh != pnut-sh-compiled-by-pnut-sh-sh.sh"
      exit 1
    fi
    printf_timing "pnut-sh.sh compiling pnut.c -> pnut-exe-compiled-by-pnut-sh-sh.sh" \
                  "$1 $TEMP_DIR/pnut-sh.sh $PNUT_EXE_OPTIONS pnut.c > $TEMP_DIR/pnut-exe-compiled-by-pnut-sh-sh.sh"
  else
    ./$TEMP_DIR/pnut-sh-compiled-by-gcc.exe $PNUT_EXE_OPTIONS pnut.c > $TEMP_DIR/pnut-exe-compiled-by-pnut-sh-sh.sh
  fi
  printf_timing "pnut-exe-compiled-by-pnut-sh-sh.sh compiling pnut.c -> pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe" \
                "$1 $TEMP_DIR/pnut-exe-compiled-by-pnut-sh-sh.sh $PNUT_EXE_OPTIONS pnut.c -o $TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe"

  chmod +x $TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe

  printf_timing "pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe compiling pnut.c -> pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe" \
                "./$TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe $PNUT_EXE_OPTIONS pnut.c -o $TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe"

  chmod +x $TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe

  if [ -s $TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe ] ; then
    if diff $TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe $TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe 2>&1 > /dev/null ; then
      printf "         SUCCESS... %s\n" "pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe == pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe"
    else
      printf "         FAILURE... %s\n" "pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe != pnut-sh-compiled-by-pnut-sh-sh.sh"
      exit 1
    fi
  else
    printf "         FAILURE... %s\n" "pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe is empty! (compiler crash?)"
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
    "$TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe" \
    "$TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe"

    printf "\n"

    sha1sum \
    "$TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe" \
    "$TEMP_DIR/pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-exe-compiled-by-pnut-sh-sh.exe"

  fi
}

# Parse the arguments
backend="x86_64_linux"  # Default to x86_64_linux
shell=                  # Defined if doing the full bootstrap using pnut.sh on Posix shell. "all" to test with all shells (slow).
safe=0                  # Whether to use safe mode when compiling pnut (adds checks at run time)
fast=0                  # Whether to use fast mode when compiling pnut (uses SH_SAVE_VARS_WITH_SET)
minimal_pnut=0          # Enable PNUT_BOOTSTRAP for bootstrapping
skip_pnut_sh=0          # Whether to skip the pnut.sh bootstrap (if set, we only compile with gcc)

while [ $# -gt 0 ]; do
  case $1 in
    --backend)              backend="$2";                        shift 2 ;;
    --shell)                shell="$2";                          shift 2 ;;
    --fast)                 fast=1;                              shift 1 ;;
    --no-pnut-sh-bootstrap) skip_pnut_sh=1;                      shift 1 ;;
    --safe)                 safe=1;                              shift 1 ;;
    --minimal-pnut)         minimal_pnut=1;                      shift 1 ;;
    --one-pass-generator)   add_exe_option "ONE_PASS_GENERATOR"; shift 1 ;;
    --stats)                add_exe_option "PRINT_MEMORY_STATS"; shift 1 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

case $backend in
  x86_64_mac | x86_64_linux | i386_linux)
    PNUT_EXE_OPTIONS="$PNUT_EXE_OPTIONS -Dtarget_$backend" ;;
  *)
    echo "Unknown backend: $backend"
    echo "Supported backends: x86_64_mac x86_64_linux i386_linux"
    exit 1
    ;;
esac

if [ $fast -eq 1 ]; then PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS_FAST";          fi
if [ $safe -eq 1 ]; then PNUT_EXE_OPTIONS="$PNUT_EXE_OPTIONS -DSAFE_MODE"; fi
if [ $minimal_pnut -eq 1 ]; then
  PNUT_EXE_OPTIONS="$PNUT_EXE_OPTIONS -DPNUT_BOOTSTRAP";
  PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS -DPNUT_BOOTSTRAP";
  PNUT_SH_OPTIONS_FAST="$PNUT_SH_OPTIONS_FAST -DPNUT_BOOTSTRAP";
fi

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
