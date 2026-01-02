#! /bin/sh
#
# ./analysis/evaluate-shell-comp-option-impact.sh
# This script evaluates the impact of the one-pass optimization, testing
# pnut-sh.sh and pnut-exe.sh with and without the parameter.

TEMP_DIR="build/eval-shell-comp-option"

if [ $# -eq 0 ]; then
  echo "Usage: $0 <options>"
  exit 1
fi

PNUT_SH_OPTIONS_UNDER_TEST="$@"
PNUT_EXE_OPTIONS="-Dtarget_i386_linux -DONE_PASS_GENERATOR"
PNUT_SH_OPTIONS="-Dsh -DRT_NO_INIT_GLOBALS"

mkdir -p "$TEMP_DIR"

printf_timing() {
  msg=$1
  cmd=$2
  real_time=`env time -p sh -c "$cmd" 2>&1 | grep '^real ' | sed 's/.* //'`
  printf "%ss %s\n" $real_time "$msg"
}

make_pnut_sh_sh() { # $1: pnut-sh (exec) compilation options, $2: pnut-sh.sh compilation options
  if [ -z "$1$2" ]; then
    id="base"
  else
    id=$(printf "%s" "$1$2" | md5sum | cut -c 1-16) # 16 characters should be enough
  fi

  gcc $1 pnut.c -o "$TEMP_DIR/pnut-sh-$id"
  ./$TEMP_DIR/pnut-sh-$id $2 pnut.c > "$TEMP_DIR/pnut-sh-$id.sh"
  echo "pnut-sh-$id.sh"
}

make_pnut_exe_sh() { # $1: pnut-sh (exec) compilation options, $2: pnut-sh.sh compilation options
  if [ -z "$1$2" ]; then
    id="base"
  else
    id=$(printf "%s" "$1$2" | md5sum | cut -c 1-16) # 16 characters should be enough
  fi

  gcc $1 pnut.c -o "$TEMP_DIR/pnut-sh-$id"
  ./$TEMP_DIR/pnut-sh-$id $2 pnut.c > "$TEMP_DIR/pnut-exe-$id.sh"
  echo "pnut-exe-$id.sh"
}

pnut_sh_base=$(make_pnut_sh_sh "$PNUT_SH_OPTIONS" "$PNUT_SH_OPTIONS") # Base
pnut_exe_base=$(make_pnut_exe_sh "$PNUT_SH_OPTIONS" "$PNUT_EXE_OPTIONS") # Base
pnut_sh_opts=$(make_pnut_sh_sh "$PNUT_SH_OPTIONS $PNUT_SH_OPTIONS_UNDER_TEST" "$PNUT_SH_OPTIONS") # With options
pnut_exe_opts=$(make_pnut_exe_sh "$PNUT_SH_OPTIONS $PNUT_SH_OPTIONS_UNDER_TEST" "$PNUT_EXE_OPTIONS") # With options

# Then for each shell, we measure the execution time:

for shell in ksh dash bash yash osh zsh; do
  # printf_timing \
  #   "pnut-sh.sh (base) pnut-sh.c with $shell" \
  #   "$shell $TEMP_DIR/$pnut_sh_base $PNUT_SH_OPTIONS pnut.c | /dev/null"

  printf_timing \
    "pnut-sh.sh (base) pnut-exe.c with $shell" \
    "$shell $TEMP_DIR/$pnut_sh_base $PNUT_EXE_OPTIONS pnut.c | /dev/null"
  printf_timing \
    "pnut-sh.sh (options) pnut-exe.c with $shell" \
    "$shell $TEMP_DIR/$pnut_sh_opts $PNUT_EXE_OPTIONS pnut.c | /dev/null"

  printf_timing \
    "pnut-exe.sh (base) pnut-exe.c with $shell" \
    "$shell $TEMP_DIR/$pnut_exe_base $PNUT_EXE_OPTIONS pnut.c | /dev/null"
  printf_timing \
    "pnut-exe.sh (options) pnut-exe.c with $shell" \
    "$shell $TEMP_DIR/$pnut_exe_opts $PNUT_EXE_OPTIONS pnut.c | /dev/null"

done
