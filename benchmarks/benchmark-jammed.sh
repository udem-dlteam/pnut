# Benchmark jammed.sh extraction:
#
# There is a small difference with this and the extraction done during the
# bootstrap, which is that this benchmark executes with the bintools already
# bootstrapped (and compiled from GCC), meaning the `mkdir` calls execute more
# quickly. This is unlikely to make a measurable difference so this is good
# enough.

TEMP_DIR="build/jammed"

mkdir -p "$TEMP_DIR"

printf_timing() {
  msg=$1
  cmd=$2
  real_time=`env time -p sh -c "$cmd" 2>&1 | grep '^real ' | sed 's/.* //'`
  printf "%ss %s\n" $real_time "$msg"
}

for shell in ksh dash bash yash osh zsh; do
  root_dir=$(pwd)
  rm -rf "$TEMP_DIR/$shell" # Make sure it's empty to start fresh
  mkdir -p "$TEMP_DIR/$shell"
  cp kit/jammed.sh "$TEMP_DIR/$shell/jammed.sh"
  cd "$TEMP_DIR/$shell"

  printf_timing \
    "jammed.sh with $shell" \
    "$shell jammed.sh --force-no-exec"
  rm jammed.sh # Remove so du doesn't count it
  cd "$root_dir"
  du -sh "$TEMP_DIR/$shell" # Make sure it properly extracted
done
