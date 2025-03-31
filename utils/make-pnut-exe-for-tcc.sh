#! /bin/sh
set -e -u

TEMP_DIR=build/tcc
PNUT_ARCH=i386_linux

if [ ! -d "$TEMP_DIR" ]; then mkdir -p "$TEMP_DIR"; fi

printf_timing() {
  msg=$1
  cmd=$2
  real_time=`env time -p sh -c "$cmd" 2>&1 | grep '^real ' | sed 's/.* //'`
  printf "%ss %s\n" $real_time "$msg"
}

: ${PNUT_OPTIONS:=} # Default to empty options

PNUT_EXE_OPTIONS="$PNUT_OPTIONS -DBOOTSTRAP_LONG -Dtarget_$PNUT_ARCH -DUNDEFINED_LABELS_ARE_RUNTIME_ERRORS"
PNUT_SH_OPTIONS="$PNUT_OPTIONS -DRT_NO_INIT_GLOBALS -Dsh"
PNUT_SH_OPTIONS_FAST="$PNUT_SH_OPTIONS -DSH_SAVE_VARS_WITH_SET -DOPTIMIZE_CONSTANT_PARAM"

# Parse the arguments
shell= # Defined if doing the full bootstrap using pnut.sh on POSIX shell. Otherwise, we use gcc to compile pnut.
safe=0 # Whether to use safe mode when compiling pnut (adds checks at run time)
fast=0 # Whether to use fast mode when compiling pnut (faster shell code)

while [ $# -gt 0 ]; do
  case $1 in
    --gcc)     use_gcc=1;                               shift 1 ;;
    --shell)   shell="$2";                              shift 2 ;;
    --fast)    PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS_FAST"; shift 1 ;;
    --safe)    safe=1;                                  shift 1 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

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

make_with_shell() { # $1 = shell

  # Test with the specified shell
  echo "Making pnut-exe from pnut-sh.sh and $1"

  # Let's assume we have a premade pnut-sh.sh script
  # In a normal bootstrap, we'd have a precompiled pnut-sh.sh script.
  # Here, let's create it using gcc
  printf_timing "1. Making pnut-sh.sh" \
                "gcc -o $TEMP_DIR/pnut-sh.exe $PNUT_SH_OPTIONS pnut.c && ./$TEMP_DIR/pnut-sh.exe $PNUT_SH_OPTIONS pnut.c > $TEMP_DIR/pnut-sh.sh"

  printf "### Here you'd audit the pnut-sh.sh script to make sure it is correct ### \n"

  printf_timing "2. pnut-sh.sh compiling pnut.c -> pnut-exe.sh" \
                "$1 $TEMP_DIR/pnut-sh.sh $PNUT_EXE_OPTIONS pnut.c > $TEMP_DIR/pnut-exe.sh"

  printf_timing "3. pnut-exe.sh compiling pnut.c -> pnut-exe" \
                "$1 $TEMP_DIR/pnut-exe.sh $PNUT_EXE_OPTIONS -DNO_BUILTIN_LIBC pnut.c -o $TEMP_DIR/pnut-exe"

  sha256sum $TEMP_DIR/pnut-exe

}

if [ -n "$shell" ]; then

  if [ "$shell" = "all" ]; then
    # Test with all shells
    for shell in ksh bash dash yash zsh; do
      if command -v $shell > /dev/null 2>&1; then
        make_with_shell $shell
      else
        echo "Shell $shell not found, skipping..."
      fi
    done
  else
    # Test with the specified shell
    make_with_shell $shell
  fi

else

  # Bootstrapping on shells can be slow, we can skip this step and use gcc.
  # We know that these 2 methods reach the same executable, so no need to use
  # the slow method when developing.

  echo "Making pnut-exe from gcc (gcc -> pnut-exe-by-gcc -> pnut-exe-by-pnut-exe)"

  gcc pnut.c $PNUT_EXE_OPTIONS -o $TEMP_DIR/pnut-exe-for-pnut-exe
  ./$TEMP_DIR/pnut-exe-for-pnut-exe $PNUT_EXE_OPTIONS -DNO_BUILTIN_LIBC pnut.c -o $TEMP_DIR/pnut-exe
  sha256sum $TEMP_DIR/pnut-exe

fi

chmod +x $TEMP_DIR/pnut-exe
