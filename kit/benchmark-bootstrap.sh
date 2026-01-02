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

for shell in ksh dash bash yash osh zsh; do
  root_dir=$(pwd)
  rm -rf "$TEMP_DIR/$shell" # Make sure it's empty to start fresh
  mkdir -p "$TEMP_DIR/$shell"
  cp kit/jammed.sh "$TEMP_DIR/$shell/jammed.sh"
  cd "$TEMP_DIR/$shell"

  chmod +x jammed.sh
  printf_timing \
    "jammed.sh with $shell" \
    "$shell jammed.sh"
  SHELL="$shell" time bash bootstrap.sh
  cd "$root_dir"
done
