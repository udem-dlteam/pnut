set -e

DIR="benchmarks/ackerman"
COMP_DIR="$DIR/compiled"

# Create the compiled directory if it doesn't exist
mkdir -p $COMP_DIR

print_time()
{
  ms=$1
  printf "%s %s\n" "$((ms/1000)).$((ms/100%10))$((ms/10%10))$((ms%10))s" "$2"
}

with_size() {
  shell=$1
  env_size=$2
  option=$3
  gcc -E -P -DARR_SIZE=$env_size $DIR/ack.c > $COMP_DIR/ack-$env_size.c

  ./benchmarks/pnut-sh.exe -D$option $COMP_DIR/ack-$env_size.c > $COMP_DIR/ack-$env_size-$option.sh

  TIME_MS=$(( `bash -c "time $shell $COMP_DIR/ack-$env_size-$option.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $TIME_MS "for: $shell with $env_size and $option"
}

sizes="1000 5000 10000 50000 100000 500000 1000000"
shells="ksh dash bash yash zsh"
options="RT_COMPACT OPTIMIZE_LONG_LINES SH_AVOID_PRINTF_USE SH_SAVE_VARS_WITH_SET OPTIMIZE_CONSTANT_PARAM"

# Compile pnut with different options
./benchmarks/compile-pnut.sh -DDRT_NO_INIT_GLOBALS

for shell in $shells; do
  for size in $sizes; do
    for option in $options; do
      with_size "$shell" $size $option
    done
  done
done
