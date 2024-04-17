#! /bin/sh

if [ -z $SHELL ]; then
  echo "SHELL not set"
  exit 1
fi

bootstrap_with_shell() {

  gcc -o pnut.exe pnut.c

  gcc -E -C -P -DPNUT_CC pnut.c > pnut-after-cpp.c

  ./pnut.exe < pnut-after-cpp.c > pnut.sh

#  echo "Bootstrap with $1"
#  time $1 pnut.sh --no-zero-globals < pnut-after-cpp.c > pnut-twice-bootstrapped.sh
#  diff pnut.sh pnut-twice-bootstrapped.sh
#  wc pnut.c pnut.sh pnut-twice-bootstrapped.sh

  gcc -E -C -P -DPNUT_CC -DX86_CODEGEN pnut.c > pnut-x86-after-cpp.c

  if : ; then

      gcc -o pnut-x86.exe -DX86_CODEGEN pnut.c
      time ./pnut-x86.exe < pnut-x86-after-cpp.c > pnut-x86-bootstrapped-with-exe.exe
      chmod +x pnut-x86-bootstrapped-with-exe.exe
      ls -l pnut-x86-bootstrapped-with-exe.exe

  fi

  if : ; then

      ./pnut.exe < pnut-x86-after-cpp.c > pnut-x86.sh
      time $1 pnut-x86.sh --no-zero-globals < pnut-x86-after-cpp.c > pnut-x86-bootstrapped-with-sh.exe
      chmod +x pnut-x86-bootstrapped-with-sh.exe
      ls -l pnut-x86-bootstrapped-with-sh.exe

  fi

  if : ; then

      gcc -E -C -P -DPNUT_CC winterpi.c > winterpi-after-cpp.c

      time ./pnut-x86.exe < winterpi-after-cpp.c > winterpi-1.exe
      chmod +x winterpi-1.exe
      ls -l winterpi-1.exe
#      ./winterpi-1.exe

  fi

  if : ; then

      gcc -E -C -P -DPNUT_CC winterpi.c > winterpi-after-cpp.c

      time $1 pnut-x86.sh --no-zero-globals < winterpi-after-cpp.c > winterpi-2.exe
      chmod +x winterpi-2.exe
      ls -l winterpi-2.exe
#      ./winterpi-2.exe

  fi

  if : ; then

      gcc -E -C -P -DPNUT_CC winterpi.c > winterpi-after-cpp.c

      ./pnut-x86-bootstrapped-with-sh.exe < winterpi-after-cpp.c > winterpi-3.exe
      chmod +x winterpi-3.exe
      ls -l winterpi-3.exe
#      ./winterpi-3.exe

  fi


  if false ; then

      lldb ./pnut.exe <<EOF
settings set target.input-path pnut-x86-after-cpp.c
run
EOF

  fi

#  time $1 pnut-x86.sh < pnut-x86-after-cpp.c > pnut-x86.exe

#  gcc -o pnut-x86.exe -DX86_CODEGEN pnut.c
#  gcc -E -C -P -DPNUT_CC params.c | ./pnut-x86.exe   # > params.exe

#  gcc -E -C -P -DPNUT_CC winterpi.c | time $1 pnut-x86.sh > winterpi.exe
#  chmod +x winterpi.exe
#  ./winterpi.exe

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
