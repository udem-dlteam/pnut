set -e

DIR="benchmarks/integer-add-mult"
COMP_DIR="$DIR/compiled"

# Create the compiled directory if it doesn't exist
mkdir -p $COMP_DIR

print_time()
{
  ms=$1
  printf "%s %s\n" "$((ms/1000)).$((ms/100%10))$((ms/10%10))$((ms%10))s" "$2"
}

with_size() {
  # shell=$1
  env_size=$2
  gcc -E -P -DARR_SIZE=$env_size $DIR/int-add-mult.c > $COMP_DIR/int-add-mult-$env_size.c

  ./benchmarks/pnut-sh.exe $COMP_DIR/int-add-mult-$env_size.c > $COMP_DIR/int-add-mult-$env_size.sh

  TIME_MS=$(( `bash -c "time $1 $COMP_DIR/int-add-mult-$env_size.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $TIME_MS "for: $1 with $env_size"
}

sizes="1000 5000 10000 50000 100000 500000 1000000"
shells="ksh dash bash yash zsh"

# Compile pnut with
./benchmarks/compile-pnut.sh -DDRT_NO_INIT_GLOBALS

for shell in $shells; do
  for size in $sizes; do
    with_size "$shell" $size
  done
done
