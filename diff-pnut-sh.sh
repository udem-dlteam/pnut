#! /bin/sh

set -e

: ${PNUT_OPTIONS:=} # Default to empty options

TEMP_DIR="build"
PNUT_SH_OPTIONS="$PNUT_OPTIONS -DRT_NO_INIT_GLOBALS -Dsh"
PNUT_SH_OPTIONS_FAST="$PNUT_SH_OPTIONS -DSH_SAVE_VARS_WITH_SET -DOPTIMIZE_CONSTANT_PARAM"
PNUT_SH_FILE_ORIGINAL="$TEMP_DIR/pnut-sh-original.sh"
PNUT_SH_FILE_FRESH="$TEMP_DIR/pnut-sh.sh"

# Parse the arguments
init=0

while [ $# -gt 0 ]; do
  case $1 in
    --fast)  PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS_FAST"; shift 1 ;;
    --init)  init=1;                                  shift 1 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

if [ ! -d "$TEMP_DIR" ]; then mkdir "$TEMP_DIR"; fi

gcc -o "$TEMP_DIR/pnut.exe" $PNUT_SH_OPTIONS pnut.c

if [ $init -eq 1 ]; then
  ./$TEMP_DIR/pnut.exe $PNUT_SH_OPTIONS "pnut.c" > "$PNUT_SH_FILE_ORIGINAL"
  exit 0
fi

if [ ! -f "$PNUT_SH_FILE_ORIGINAL" ]; then
  echo "$PNUT_SH_FILE_ORIGINAL not found. Run this script with --init first."
  exit 1
fi
./$TEMP_DIR/pnut.exe $PNUT_SH_OPTIONS "pnut.c" > "$PNUT_SH_FILE_FRESH"
diff -w "$PNUT_SH_FILE_ORIGINAL" "$PNUT_SH_FILE_FRESH"
