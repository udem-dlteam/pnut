set -e

DIR="benchmarks/base64"
COMP_DIR="$DIR/compiled"
INPUT_DIR="$DIR/inputs"

# Create the compiled and input directories if they don't exist
mkdir -p $COMP_DIR
mkdir -p $INPUT_DIR

print_time() {
  ms=$1
  printf "%s %s\n" "$((ms/1000)).$((ms/100%10))$((ms/10%10))$((ms%10))s" "$2"
}

# Generate input files of different sizes with a seed
generate_input_files() {
  seed=$1
  for len in $lengths; do
    openssl enc -aes-256-ctr -pass pass:"$seed" -nosalt </dev/zero 2>/dev/null | head -c $len > "$INPUT_DIR/input-$len.txt"
  done
}

with_size() {
  shell=$1
  len=$2
  option=$3

  gcc -E -P -DBUF_SIZE=$len $DIR/base64.c > $COMP_DIR/base64-$len.c

  ./benchmarks/pnut-sh.exe -D$option $COMP_DIR/base64-$len.c > $COMP_DIR/base64-$len-$option.sh

  TIME_MS=$(( `bash -c "time $shell $COMP_DIR/base64-$len-$option.sh < $INPUT_DIR/input-$len.txt" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $TIME_MS "for: $shell with BUFFSIZE=$len and $option"
}

sizes="1024 4096 8192 16384 32768"
shells="ksh dash bash yash zsh"
options="RT_COMPACT OPTIMIZE_LONG_LINES SH_AVOID_PRINTF_USE SH_SAVE_VARS_WITH_SET OPTIMIZE_CONSTANT_PARAM"

# Generate input files
seed=12345
generate_input_files $seed

# Compile pnut with different options
./benchmarks/compile-pnut.sh -DDRT_NO_INIT_GLOBALS

for shell in $shells; do
  for size in $sizes; do
    for option in $options; do
      with_size "$shell" $size $option
    done
  done
done
