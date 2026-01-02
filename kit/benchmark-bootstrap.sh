# Benchmark jammed.sh extraction + bootstrap:

TEMP_DIR="build/bootstrap"

mkdir -p "$TEMP_DIR"

printf_timing() {
  msg=$1
  cmd=$2
  real_time=`env time -p sh -c "$cmd" 2>&1 | grep '^real ' | sed 's/.* //'`
  printf "%ss %s\n" $real_time "$msg"
}

# Make sure the jammed.sh script is up-to-date
./kit/make-jammed.sh

for BOOTSTRAP_SHELL in ksh dash bash yash osh zsh; do
  root_dir=$(pwd)
  rm -rf "$TEMP_DIR/$BOOTSTRAP_SHELL" # Make sure it's empty to start fresh
  mkdir -p "$TEMP_DIR/$BOOTSTRAP_SHELL"
  cp kit/jammed.sh "$TEMP_DIR/$BOOTSTRAP_SHELL/jammed.sh"

  # Skip ahead to TCC bootstrap
  # gcc pnut.c -o $TEMP_DIR/$BOOTSTRAP_SHELL/pnut-exe -Dtarget_i386_linux -DONE_PASS_GENERATOR -DBOOTSTRAP_TCC

  cd "$TEMP_DIR/$BOOTSTRAP_SHELL"
  chmod +x jammed.sh
  printf_timing \
    "jammed.sh with $BOOTSTRAP_SHELL" \
    "$BOOTSTRAP_SHELL jammed.sh"
  BOOTSTRAP_SHELL="$BOOTSTRAP_SHELL" time bash bootstrap.sh
  cd "$root_dir"
done
