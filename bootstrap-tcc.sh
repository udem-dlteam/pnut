#! /bin/sh
set -e -u

fail() { echo "❌ $1"; exit 1; }

TEMP_DIR="build/tcc"

: ${PNUT_OPTIONS:=} # Default to empty options

PNUT_EXE_OPTIONS="$PNUT_OPTIONS -Dtarget_x86_64_linux -DUNDEFINED_LABELS_ARE_RUNTIME_ERRORS -DSAFE_MODE" # Backend is set by the backend option

if [ ! -d "$TEMP_DIR" ]; then mkdir -p "$TEMP_DIR"; fi

printf_timing() {
  msg=$1; shift
  cmd=$@
  real_time=`env time -p sh -c "$cmd" 2>&1 | grep '^real ' | sed 's/.* //'`
  printf "%ss %s\n" $real_time "$msg"
}

# First we compile pnut with gcc
printf_timing "gcc compiling pnut-exe (with builtin libc) -> pnut-for-pnut" \
  "gcc pnut.c $PNUT_EXE_OPTIONS -o $TEMP_DIR/pnut-for-pnut"

printf_timing "gcc compiling pnut-exe (no builtin libc)  -> pnut-for-tcc" \
  "gcc pnut.c $PNUT_EXE_OPTIONS -DNO_BUILTIN_LIBC -o $TEMP_DIR/pnut-for-tcc"

# Then we compile pnut with pnut
printf_timing "pnut-exe compiling pnut-exe -> pnut-by-pnut" \
  "./$TEMP_DIR/pnut-for-pnut pnut.c $PNUT_EXE_OPTIONS -DNO_BUILTIN_LIBC -DSAFE_MODE > $TEMP_DIR/pnut-by-pnut"

chmod +x $TEMP_DIR/pnut-by-pnut

# Then we can finally compile TCC, this assumes that TCC is at ../tinycc/tcc.c
TCC_OPTIONS="-DONE_SOURCE -DCONFIG_TCC_STATIC -DTCC_TARGET_X86_64"
TCC_OPTIONS_PORTABLE_LIBC="-Iportable_libc/include/ $TCC_OPTIONS"

printf_timing "pnut-for-tcc compiling tcc -> tcc-by-pnut" \
  "./$TEMP_DIR/pnut-for-tcc ../tinycc/tcc.c $TCC_OPTIONS_PORTABLE_LIBC > $TEMP_DIR/tcc-by-pnut"

printf_timing "pnut-by-pnut compiling tcc -> tcc-by-pnut-by-pnut" \
  "./$TEMP_DIR/pnut-by-pnut ../tinycc/tcc.c $TCC_OPTIONS_PORTABLE_LIBC > $TEMP_DIR/tcc-by-pnut-by-pnut"

if diff "$TEMP_DIR/tcc-by-pnut" "$TEMP_DIR/tcc-by-pnut-by-pnut"; then
  echo "TCC compiled correctly by both pnut and pnut-by-pnut"
  sha256sum "$TEMP_DIR/tcc-by-pnut" "$TEMP_DIR/tcc-by-pnut-by-pnut"
else
  echo "TCC did not compile correctly"
  exit 1
fi
