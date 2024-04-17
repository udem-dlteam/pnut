#! /bin/sh

if [ -z $SHELL ]; then
  echo "SHELL not set"
  exit 1
fi

bootstrap_with_shell() {

  echo "===================== Bootstrap with $1"

  gcc -o pnut-sh.exe pnut.c

  gcc -E -P -DPNUT_CC pnut.c > pnut-sh-after-cpp.c

  ./pnut-sh.exe < pnut-sh-after-cpp.c > pnut-sh.sh

  gcc -E -P -DPNUT_CC -DX86_CODEGEN pnut.c > pnut-x86-after-cpp.c

  time $1 pnut-sh.sh --no-zero-globals < pnut-sh-after-cpp.c > pnut-sh-compiled-by-pnut-sh.sh

  time $1 pnut-sh-compiled-by-pnut-sh.sh --no-zero-globals < pnut-x86-after-cpp.c > pnut-x86-compiled-by-pnut-sh.sh

  time $1 pnut-x86-compiled-by-pnut-sh.sh --no-zero-globals < pnut-x86-after-cpp.c > pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-sh.exe

  chmod +x pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-sh.exe

  time ./pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-sh.exe < pnut-x86-after-cpp.c > pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-sh.exe

  chmod +x pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-sh.exe

  ls -l \
  pnut-sh.exe \
  pnut-sh.sh \
  pnut-sh-compiled-by-pnut-sh.sh \
  pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-sh.exe \
  pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-sh.exe

  sha1sum \
  pnut-sh.sh \
  pnut-sh-compiled-by-pnut-sh.sh \
  pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-sh.exe \
  pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-x86-compiled-by-pnut-sh.exe
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
