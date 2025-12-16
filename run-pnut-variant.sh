#! /bin/sh

set -e

: ${PNUT_OPTIONS:=} # Default to empty options

TEMP_DIR="build"
PNUT_VARIANT_OPTIONS=""

run_with_shell() {

  echo "Running $variant with $1"

  env time $1 "$TEMP_DIR/pnut-$variant.sh" $PNUT_OPTIONS -Dtarget_sh "pnut.c" > "$TEMP_DIR/pnut-$variant.output"
}

# Parse the arguments
shell="$SHELL" # Use current shell as the default. "all" to test all shells.
variant=""

while [ $# -gt 0 ]; do
  case $1 in
    --shell)      shell="$2";                                               shift 2 ;;
    --fast)       PNUT_OPTIONS="$PNUT_OPTIONS -DSH_SAVE_VARS_WITH_SET";     shift 1 ;;
    --minimal-pnut) PNUT_OPTIONS="$PNUT_OPTIONS -DPNUT_BOOTSTRAP";          shift 1 ;;
    --reader)     PNUT_VARIANT_OPTIONS="-DDEBUG_GETCHAR";  variant=${1#--}; shift 1 ;;
    --tokenizer)  PNUT_VARIANT_OPTIONS="-DDEBUG_CPP";      variant=${1#--}; shift 1 ;;
    --parser)     PNUT_VARIANT_OPTIONS="-DDEBUG_PARSER";   variant=${1#--}; shift 1 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

pnut_exec="$TEMP_DIR/pnut-$variant"

if [ ! -d "$TEMP_DIR" ]; then mkdir "$TEMP_DIR"; fi

gcc -o "$pnut_exec" $PNUT_OPTIONS -Dtarget_sh pnut.c
$pnut_exec $PNUT_OPTIONS $PNUT_VARIANT_OPTIONS "pnut.c" > "$TEMP_DIR/pnut-$variant.sh"

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
