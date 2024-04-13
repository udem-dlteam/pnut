#! /bin/sh

if [ -z $SHELL ]; then
  echo "SHELL not set"
  exit 1
fi

bootstrap_with_shell() {

  gcc -o pnut.exe pnut.c

  gcc -E -C -P -DPNUT_CC pnut.c > pnut-after-cpp.c

  ./pnut.exe < pnut-after-cpp.c > pnut.sh

  echo "Bootstrap with $1"

  time $1 pnut.sh --no-zero-globals < pnut-after-cpp.c > pnut-twice-bootstrapped.sh

  diff pnut.sh pnut-twice-bootstrapped.sh

  wc pnut.c pnut.sh pnut-twice-bootstrapped.sh
}

# Handle runtime options
TEST_ALL_SHELLS=0

if [ $# -gt 0 ] && [ $1 = "TEST_ALL_SHELLS" ] ; then TEST_ALL_SHELLS=1; shift; fi

bootstrap_with_shell "ksh"

if [ $TEST_ALL_SHELLS -ne 0 ]; then
  bootstrap_with_shell "dash"
  bootstrap_with_shell "bash"
  bootstrap_with_shell "zsh"
  bootstrap_with_shell "yash"
  bootstrap_with_shell "mksh"
fi
