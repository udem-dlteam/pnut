#! /bin/sh

set -e

: ${PNUT_OPTIONS:=} # Default to empty options

TEMP_DIR="build"
PNUT_SH_OPTIONS="$PNUT_OPTIONS -Dsh"
PNUT_SH_OPTIONS_FAST="$PNUT_SH_OPTIONS -DSH_SAVE_VARS_WITH_SET"

bootstrap_with_shell() {

  echo "Bootstrap with $1"

  env time $1 "$TEMP_DIR/pnut-sh.sh" $PNUT_SH_OPTIONS "pnut.c" > "$TEMP_DIR/pnut-sh-twice-bootstrapped.sh"

  diff "$TEMP_DIR/pnut-sh.sh" "$TEMP_DIR/pnut-sh-twice-bootstrapped.sh"

  wc pnut.c "$TEMP_DIR/pnut-sh.sh" "$TEMP_DIR/pnut-sh-twice-bootstrapped.sh"
}

# Parse the arguments
shell="$SHELL" # Use current shell as the default. "all" to test all shells.
safe=0
fast=0
minimal_pnut=0 # Enable PNUT_BOOTSTRAP for bootstrapping
compile_only=0

while [ $# -gt 0 ]; do
  case $1 in
    --shell) shell="$2";                              shift 2 ;;
    --fast)  fast=1;                                  shift 1 ;;
    --safe)  safe=1;                                  shift 1 ;;
    --minimal-pnut) minimal_pnut=1;                   shift 1 ;;
    --compile-only) compile_only=1;                   shift 1 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

if [ ! -d "$TEMP_DIR" ]; then mkdir "$TEMP_DIR"; fi

if [ $fast -eq 1 ]; then PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS_FAST"; fi
if [ $safe -eq 1 ]; then PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS -DSAFE_MODE"; fi
if [ $minimal_pnut -eq 1 ]; then PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS -DPNUT_BOOTSTRAP"; fi

# Clean temporary directory to start fresh
# For some reason, on MacOS, certain executables act differently if they
# existed before being recompiled, as if the operating system cached some
# information about them and didn't invalidate the cache when they were
# recompiled, or initialized the memory differently if the executable existed
# before being recompiled.
rm -rf $TEMP_DIR/*

gcc -o "$TEMP_DIR/pnut.exe" $PNUT_SH_OPTIONS pnut.c

./$TEMP_DIR/pnut.exe $PNUT_SH_OPTIONS "pnut.c" > "$TEMP_DIR/pnut-sh.sh" || {
  echo "Failed to compile pnut"
  tail -n 20 "$TEMP_DIR/pnut-sh.sh"
  exit 1
}

# Exit now if we only want to compile
if [ $compile_only -eq 1 ]; then
  echo "Compiled pnut.sh successfully"
  exit 0;
fi

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
