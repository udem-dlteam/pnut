#! /bin/sh

set -e

: ${PNUT_OPTIONS:=} # Default to empty options

TEMP_DIR="bootstrap-results"
PNUT_SH_OPTIONS="$PNUT_OPTIONS -DRT_NO_INIT_GLOBALS -Dsh"
PNUT_SH_OPTIONS_FAST="$PNUT_SH_OPTIONS -DSH_SAVE_VARS_WITH_SET -DOPTIMIZE_CONSTANT_PARAM"

bootstrap_with_shell() {

  echo "Bootstrap with $1"

  time $1 "$TEMP_DIR/pnut.sh" $PNUT_SH_OPTIONS "pnut.c" > "$TEMP_DIR/pnut-twice-bootstrapped.sh"

  diff "$TEMP_DIR/pnut.sh" "$TEMP_DIR/pnut-twice-bootstrapped.sh"

  wc pnut.c "$TEMP_DIR/pnut.sh" "$TEMP_DIR/pnut-twice-bootstrapped.sh"
}

# Parse the arguments
shell="$SHELL" # Use current shell as the default. "all" to test all shells.

while [ $# -gt 0 ]; do
  case $1 in
    --shell) shell="$2"; shift 2 ;;
    --fast)  PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS_FAST"; shift 1 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

if [ ! -d "$TEMP_DIR" ]; then mkdir "$TEMP_DIR"; fi

gcc -o "$TEMP_DIR/pnut.exe" $PNUT_SH_OPTIONS pnut.c

./$TEMP_DIR/pnut.exe $PNUT_SH_OPTIONS "pnut.c" > "$TEMP_DIR/pnut.sh"

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
