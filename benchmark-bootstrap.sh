#!/bin/sh

SHELL_TO_TEST="$1"

if [ "$SHELL_TO_TEST" = "" ] ; then
  echo "usage: $0 <shell_to_test>"
  exit 1
fi

TEMP_DIR="bootstrap-results"
PNUT_SH_OPTIONS="-DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Dsh"
PNUT_I386_OPTIONS="-DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Di386"

echo "PLATFORM: `uname -a`"
echo "SHELL: $SHELL_TO_TEST"

print_time()
{
  ms=$1
  printf "%s %s\n" "$((ms/1000)).$((ms/100%10))$((ms/10%10))$((ms%10))s" "$2"
}

if which gcc > /dev/null ; then
  PNUT_SH_COMPILED_BY_GCC_MS=$(( `bash -c "time gcc -o $TEMP_DIR/pnut-sh-compiled-by-gcc.exe $PNUT_SH_OPTIONS pnut.c" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  PNUT_SH_COMPILED_BY_PNUT_SH_COMPILED_BY_GCC_MS=$(( `bash -c "time $TEMP_DIR/pnut-sh-compiled-by-gcc.exe $PNUT_SH_OPTIONS pnut.c > $TEMP_DIR/pnut-sh.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $PNUT_SH_COMPILED_BY_GCC_MS "for: gcc $PNUT_SH_OPTIONS pnut.c -o pnut-sh-compiled-by-gcc.exe"
  print_time $PNUT_SH_COMPILED_BY_PNUT_SH_COMPILED_BY_GCC_MS "for: pnut-sh-compiled-by-gcc.exe $PNUT_SH_OPTIONS pnut.c > pnut-sh.sh"
else
  if [ ! -e $TEMP_DIR/pnut-sh.sh ] ; then
    echo "*** A prebuilt $TEMP_DIR/pnut-sh.sh is required"
    exit 1
  fi
fi

PNUT_SH_COMPILED_BY_PNUT_SH_SH_MS=$(( `bash -c "time $SHELL_TO_TEST $TEMP_DIR/pnut-sh.sh $PNUT_SH_OPTIONS pnut.c > $TEMP_DIR/pnut-sh-compiled-by-pnut-sh-sh.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))

print_time $PNUT_SH_COMPILED_BY_PNUT_SH_SH_MS "for: $SHELL_TO_TEST pnut-sh.sh $PNUT_SH_OPTIONS pnut.c > pnut-sh-compiled-by-pnut-sh-sh.sh"

if ! diff $TEMP_DIR/pnut-sh-compiled-by-pnut-sh-sh.sh $TEMP_DIR/pnut-sh.sh > /dev/null ; then
  echo "*** pnut-sh-compiled-by-pnut-sh-sh.sh != pnut-sh.sh"
fi

PNUT_I386_COMPILED_BY_PNUT_SH_SH_MS=$(( `bash -c "time $SHELL_TO_TEST $TEMP_DIR/pnut-sh.sh $PNUT_I386_OPTIONS pnut.c > $TEMP_DIR/pnut-i386-compiled-by-pnut-sh-sh.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))

print_time $PNUT_I386_COMPILED_BY_PNUT_SH_SH_MS "for: $SHELL_TO_TEST pnut-sh.sh $PNUT_I386_OPTIONS pnut.c > pnut-i386-compiled-by-pnut-sh-sh.sh"

PNUT_I386_COMPILED_BY_PNUT_I386_SH_MS=$(( `bash -c "time $SHELL_TO_TEST $TEMP_DIR/pnut-i386-compiled-by-pnut-sh-sh.sh $PNUT_I386_OPTIONS pnut.c > $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-sh.exe" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))

print_time $PNUT_I386_COMPILED_BY_PNUT_I386_SH_MS "for: $SHELL_TO_TEST pnut-i386-compiled-by-pnut-sh-sh.sh $PNUT_I386_OPTIONS pnut.c > pnut-i386-compiled-by-pnut-i386-sh.exe"

chmod +x $TEMP_DIR/pnut-i386-compiled-by-pnut-sh-sh.exe

PNUT_I386_COMPILED_BY_PNUT_I386_EXE_MS=$(( `bash -c "time $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-sh.exe $PNUT_I386_OPTIONS pnut.c > $TEMP_DIR/pnut-i386-compiled-pnut-i386-exe.exe" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))

print_time $PNUT_I386_COMPILED_BY_PNUT_I386_EXE_MS "for: pnut-i386-compiled-by-pnut-i386-sh.exe $PNUT_I386_OPTIONS pnut.c > pnut-i386-compiled-pnut-i386-exe.exe"

if ! diff $TEMP_DIR/pnut-i386-compiled-pnut-i386-exe.exe $TEMP_DIR/pnut-i386-compiled-by-pnut-i386-sh.exe > /dev/null ; then
  echo "*** pnut-i386-compiled-pnut-i386-exe.exe != pnut-i386-compiled-by-pnut-i386-sh.exe"
fi
