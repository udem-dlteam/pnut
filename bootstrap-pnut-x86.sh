#! /bin/sh

TEMP_DIR="bootstrap-results"

PNUT_x86_OPTIONS="-Dx86_64"

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

  gcc -o $TEMP_DIR/pnut-x86-by-gcc.exe $PNUT_x86_OPTIONS pnut.c
  gcc -E -P -DPNUT_CC $PNUT_x86_OPTIONS pnut.c > "$TEMP_DIR/pnut-after-cpp.c"
  ./$TEMP_DIR/pnut-x86-by-gcc.exe < "$TEMP_DIR/pnut-after-cpp.c" > $TEMP_DIR/pnut-x86-by-pnut-x86-by-gcc.exe

  chmod +x $TEMP_DIR/pnut-x86-by-pnut-x86-by-gcc.exe

  ./$TEMP_DIR/pnut-x86-by-pnut-x86-by-gcc.exe < "$TEMP_DIR/pnut-after-cpp.c" > $TEMP_DIR/pnut-x86-by-pnut-x86-by-pnut-x86-by-gcc.exe

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

  gcc -o $TEMP_DIR/pnut-sh-compiled-by-gcc.exe -Dsh pnut.c
  gcc -E -P -DPNUT_CC -Dsh pnut.c > "$TEMP_DIR/pnut-sh-after-cpp.c"
  ./$TEMP_DIR/pnut-sh-compiled-by-gcc.exe < "$TEMP_DIR/pnut-sh-after-cpp.c" > $TEMP_DIR/pnut-sh.sh

  # create pnut-i386.sh, the C to i386 machine code compiler as a shell script

  gcc -E -P -DPNUT_CC $PNUT_x86_OPTIONS pnut.c > $TEMP_DIR/pnut-i386-after-cpp.c
  printf_timing "pnut-sh.sh compiling pnut.c -> pnut-sh-compiled-by-pnut-sh-sh.sh" \
                "$1 $TEMP_DIR/pnut-sh.sh --no-zero-globals < $TEMP_DIR/pnut-sh-after-cpp.c > $TEMP_DIR/pnut-sh-compiled-by-pnut-sh-sh.sh"
  if diff $TEMP_DIR/pnut-sh.sh $TEMP_DIR/pnut-sh-compiled-by-pnut-sh-sh.sh 2>&1 > /dev/null ; then
    printf "         SUCCESS... %s\n" "pnut-sh.sh == pnut-sh-compiled-by-pnut-sh-sh.sh"
  else
    printf "         FAILURE... %s\n" "pnut-sh.sh != pnut-sh-compiled-by-pnut-sh-sh.sh"
    exit 1
  fi
  printf_timing "pnut-sh.sh compiling pnut.c -> pnut-i386-compiled-by-pnut-sh-sh.sh" \
                "$1 $TEMP_DIR/pnut-sh.sh --no-zero-globals < $TEMP_DIR/pnut-i386-after-cpp.c > $TEMP_DIR/pnut-i386-compiled-by-pnut-sh-sh.sh"
  printf_timing "pnut-i386-compiled-by-pnut-sh-sh.sh compiling pnut.c -> pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe" \
                "$1 $TEMP_DIR/pnut-i386-compiled-by-pnut-sh-sh.sh --no-zero-globals < $TEMP_DIR/pnut-i386-after-cpp.c > $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe"

  chmod +x $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe

  printf_timing "pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe compiling pnut.c -> pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe" \
                "./$TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe < $TEMP_DIR/pnut-i386-after-cpp.c > $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-i386-compiled-by-pnut-sh-sh.exe"

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

# Handle runtime options
WITH_SHELL=0
TEST_ALL_SHELLS=0

if [ $# -gt 0 ] && [ $1 = "WITH_SHELL" ] ; then WITH_SHELL=1; shift; fi
if [ $# -gt 0 ] && [ $1 = "TEST_ALL_SHELLS" ] ; then TEST_ALL_SHELLS=1; shift; fi

if [ $WITH_SHELL -ne 0 ]; then
  bootstrap_with_shell "ksh"
else
  bootstrap_with_gcc # Much faster than bootstrapping on the Shell
fi

if [ $TEST_ALL_SHELLS -ne 0 ]; then
  bootstrap_with_shell "dash"
  bootstrap_with_shell "bash"
  bootstrap_with_shell "zsh"
  bootstrap_with_shell "yash"
  bootstrap_with_shell "mksh"
fi
