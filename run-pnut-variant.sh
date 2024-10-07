#! /bin/sh

set -e

: ${PNUT_OPTIONS:=} # Default to empty options

TEMP_DIR="build"
PNUT_SH_OPTIONS="$PNUT_OPTIONS -DRT_NO_INIT_GLOBALS -Dsh"
PNUT_SH_OPTIONS_FAST="$PNUT_SH_OPTIONS -DSH_SAVE_VARS_WITH_SET -DOPTIMIZE_CONSTANT_PARAM"
PNUT_VARIANT_OPTIONS=""

run_with_shell() {

  echo "Running $variant with $1"

  time $1 "$TEMP_DIR/pnut-$variant.sh" $PNUT_SH_OPTIONS "pnut.c" > "$TEMP_DIR/pnut-$variant.output"
}

# Parse the arguments
shell="$SHELL" # Use current shell as the default. "all" to test all shells.
variant=""

while [ $# -gt 0 ]; do
  case $1 in
    --shell)      shell="$2";                                                    shift 2 ;;
    --fast)       PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS_FAST";                       shift 1 ;;
    --reader)     PNUT_VARIANT_OPTIONS="-DDEBUG_GETCHAR -Ush";  variant=${1#--}; shift 1 ;;
    --tokenizer)  PNUT_VARIANT_OPTIONS="-DDEBUG_CPP -Ush";      variant=${1#--}; shift 1 ;;
    --parser)     PNUT_VARIANT_OPTIONS="-DDEBUG_PARSER -Ush";   variant=${1#--}; shift 1 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

pnut_exec="$TEMP_DIR/pnut-$variant"

if [ ! -d "$TEMP_DIR" ]; then mkdir "$TEMP_DIR"; fi

gcc -o "$pnut_exec" $PNUT_SH_OPTIONS pnut.c
$pnut_exec $PNUT_SH_OPTIONS $PNUT_VARIANT_OPTIONS "pnut.c" > "$TEMP_DIR/pnut-$variant.sh"

if [ "$shell" = "all" ]; then
  set +e # Don't exit on error because we want to test all shells.
  run_with_shell "dash"
  run_with_shell "ksh"
  run_with_shell "bash"
  run_with_shell "yash"
  run_with_shell "mksh"
  run_with_shell "zsh"
else
  run_with_shell "$shell"
fi
