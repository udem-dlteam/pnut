set -e

DIR="benchmarks/word-count"
COMP_DIR="$DIR/compiled"
INPUT_DIR="$DIR/inputs"

# Create the compiled and input directories if they don't exist
mkdir -p $COMP_DIR
mkdir -p $INPUT_DIR

print_time() {
  ms=$1
  printf "%s %s\n" "$((ms/1000)).$((ms/100%10))$((ms/10%10))$((ms%10))s" "$2"
}

# Generate input files of different sizes
generate_input_files() {
  for len in $sizes; do
    base64 /dev/urandom | head -c $len > "$INPUT_DIR/input-$len.txt"
  done
}

with_size() {
  shell=$1
  len=$2
  option=$3

  gcc -E -P -DINPUT_FILE="\"$INPUT_DIR/input-$len.txt\"" $DIR/wc.c > $COMP_DIR/wc-$len.c

  ./benchmarks/pnut-sh.exe -D$option $COMP_DIR/wc-$len.c > $COMP_DIR/wc-$len-$option.sh

  TIME_MS=$(( `bash -c "time $shell $COMP_DIR/wc-$len-$option.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $TIME_MS "for: $shell with file size $len and $option"
}

sizes="1000 5000 10000 50000 100000 500000 1000000"
shells="ksh dash bash yash zsh"
options="RT_COMPACT OPTIMIZE_LONG_LINES SH_AVOID_PRINTF_USE SH_SAVE_VARS_WITH_SET OPTIMIZE_CONSTANT_PARAM"

# Generate input files
generate_input_files

# Compile pnut with different options
./benchmarks/compile-pnut.sh -DDRT_NO_INIT_GLOBALS

for shell in $shells; do
  for size in $sizes; do
    for option in $options; do
      with_size "$shell" $size $option
    done
  done
done
