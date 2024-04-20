#! /bin/sh

TEMP_DIR="bootstrap-results"

if [ ! -d "$TEMP_DIR" ]; then mkdir "$TEMP_DIR"; fi

gcc -o "$TEMP_DIR/pnut.exe" -Dsh pnut.c

gcc -E -C -P -DPNUT_CC -Dsh pnut.c > "$TEMP_DIR/pnut-after-cpp.c"

./$TEMP_DIR/pnut.exe "$TEMP_DIR/pnut-after-cpp.c" > "$TEMP_DIR/pnut.sh"

bootstrap_with_shell() {

  echo "Bootstrap with $1"

  time $1 "$TEMP_DIR/pnut.sh" --no-zero-globals "$TEMP_DIR/pnut-after-cpp.c" > "$TEMP_DIR/pnut-twice-bootstrapped.sh"

  diff "$TEMP_DIR/pnut.sh" "$TEMP_DIR/pnut-twice-bootstrapped.sh"

  wc pnut.c "$TEMP_DIR/pnut.sh" "$TEMP_DIR/pnut-twice-bootstrapped.sh"
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
