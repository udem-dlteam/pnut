#! /bin/sh
#
# ./analysis/evaluate-one-pass-impact.sh
# This script evaluates the impact of the one-pass optimization, testing pnut-sh
# and pnut-exe with and without the memory saving brought by the one-pass code
# generator.
#
# To measure the number of variables used during execution, the `set | wc -l` is
# used to count the number of environment variables before and after the execution
# of the program.

TEMP_DIR="build/one-pass"

PNUT_EXE_OPTIONS="-Dtarget_i386_linux -DPNUT_BOOTSTRAP -DONE_PASS_GENERATOR"
PNUT_EXE_OPTIONS_ONE_PASS_DISABLED="$PNUT_EXE_OPTIONS -DONE_PASS_GENERATOR_NO_EARLY_OUTPUT"
PNUT_SH_OPTIONS="-Dtarget_sh -DPNUT_BOOTSTRAP"
PNUT_SH_OPTIONS_ONE_PASS_DISABLED="$PNUT_SH_OPTIONS -DONE_PASS_GENERATOR_NO_EARLY_OUTPUT"

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

measure_memory_usage() { # $1: file to measure, $2: output file
  echo '__ENV_VARS_BEFORE=$(set | wc -l)' > "$2"
  head -n -1 "$1" >> "$2"
  echo '__ENV_VARS_AFTER=$(set | wc -l)' >> "$2"
  echo 'printf "# %d vars\n" "$((__ENV_VARS_AFTER - __ENV_VARS_BEFORE))" >&2' >> "$2"
}

# First we compute the memory usage of pnut-sh with and without the optimization

pnut_sh_base=$(make_pnut_sh_sh "$PNUT_SH_OPTIONS" "$PNUT_SH_OPTIONS") # Base (one-pass)
measure_memory_usage "$TEMP_DIR/$pnut_sh_base" "$TEMP_DIR/profiled-$pnut_sh_base"
pnut_sh_no_one_pass=$(make_pnut_sh_sh "$PNUT_SH_OPTIONS" "$PNUT_SH_OPTIONS_ONE_PASS_DISABLED") # Base (no one-pass)
measure_memory_usage "$TEMP_DIR/$pnut_sh_no_one_pass" "$TEMP_DIR/profiled-$pnut_sh_no_one_pass"

printf "# Number of shell variables used for: \n"
printf "# pnut-sh.sh (one-pass) pnut-sh.c: "
ksh $TEMP_DIR/profiled-$pnut_sh_base $PNUT_SH_OPTIONS pnut.c > /dev/null
printf "# pnut-sh.sh (one-pass) pnut-exe.c: "
ksh $TEMP_DIR/profiled-$pnut_sh_base $PNUT_EXE_OPTIONS pnut.c > /dev/null

printf "# pnut-sh.sh (no one-pass) pnut-sh.c: "
ksh $TEMP_DIR/profiled-$pnut_sh_no_one_pass $PNUT_SH_OPTIONS pnut.c > /dev/null
printf "# pnut-sh.sh (no one-pass) pnut-exe.c: "
ksh $TEMP_DIR/profiled-$pnut_sh_no_one_pass $PNUT_EXE_OPTIONS pnut.c > /dev/null

# Then do the same for pnut-exe

pnut_exe_base=$(make_pnut_exe_sh "$PNUT_SH_OPTIONS" "$PNUT_EXE_OPTIONS") # Base (one-pass)
measure_memory_usage "$TEMP_DIR/$pnut_exe_base" "$TEMP_DIR/profiled-$pnut_exe_base"
pnut_exe_no_one_pass=$(make_pnut_exe_sh "$PNUT_SH_OPTIONS" "$PNUT_EXE_OPTIONS_ONE_PASS_DISABLED") # Base (no one-pass)
measure_memory_usage "$TEMP_DIR/$pnut_exe_no_one_pass" "$TEMP_DIR/profiled-$pnut_exe_no_one_pass"

printf "# pnut-exe.sh (one-pass) pnut-exe.c: "
ksh $TEMP_DIR/profiled-$pnut_exe_base $PNUT_EXE_OPTIONS pnut.c > /dev/null
printf "# pnut-exe.sh (no one-pass) pnut-exe.c: "
ksh $TEMP_DIR/profiled-$pnut_exe_no_one_pass $PNUT_EXE_OPTIONS pnut.c > /dev/null

# Then for each shell, we measure the execution time:

for shell in ksh dash bash yash osh zsh; do
  printf_timing \
    "pnut-sh.sh (one-pass) pnut-sh.c with $shell" \
    "$shell $TEMP_DIR/$pnut_sh_base $PNUT_SH_OPTIONS pnut.c | sha256sum"
  printf_timing \
    "pnut-sh.sh (one-pass) pnut-exe.c with $shell" \
    "$shell $TEMP_DIR/$pnut_sh_base $PNUT_EXE_OPTIONS pnut.c | sha256sum"

  printf_timing \
    "pnut-sh.sh (not one-pass) pnut-sh.c with $shell" \
    "$shell $TEMP_DIR/$pnut_sh_no_one_pass $PNUT_SH_OPTIONS pnut.c | sha256sum"
  printf_timing \
    "pnut-sh.sh (not one-pass) pnut-exe.c with $shell" \
    "$shell $TEMP_DIR/$pnut_sh_no_one_pass $PNUT_EXE_OPTIONS pnut.c | sha256sum"

  printf_timing \
    "pnut-exe.sh (one-pass) pnut-exe.c with $shell" \
    "$shell $TEMP_DIR/$pnut_exe_base $PNUT_EXE_OPTIONS pnut.c | sha256sum"
  printf_timing \
    "pnut-exe.sh (no one-pass) pnut-exe.c with $shell" \
    "$shell $TEMP_DIR/$pnut_exe_no_one_pass $PNUT_EXE_OPTIONS pnut.c | sha256sum"
done
