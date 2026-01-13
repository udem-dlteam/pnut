set -e

DIR="benchmarks/concatenation"
COMP_DIR="$DIR/compiled"
INPUT_DIR="$DIR/inputs"

# Create the compiled and input directories if they don't exist
mkdir -p $COMP_DIR
mkdir -p $INPUT_DIR

print_time()
{
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

  TIME_MS=$(( `bash -c "time $shell $COMP_DIR/cat-base.sh $INPUT_DIR/input-$len.txt" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $TIME_MS "for: $shell with file size $len and base"

  TIME_MS=$(( `bash -c "time $shell $COMP_DIR/cat-optimized.sh $INPUT_DIR/input-$len.txt" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $TIME_MS "for: $shell with FILESIZE=$len and optimized"
}

lengths="1000 2000 5000 10000 20000"
shells="dash bash yash zsh ksh"

# Generate input files
seed=12345
generate_input_files $seed

# Compile pnut with
PNUT_OPTIONS="-DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Dsh"
gcc -o $COMP_DIR/pnut-sh-base.exe $PNUT_OPTIONS pnut.c
gcc -o $COMP_DIR/pnut-sh-optimized.exe $PNUT_OPTIONS -DOPTIMIZE_LONG_LINES pnut.c

./$COMP_DIR/pnut-sh-base.exe $DIR/cat.c > $COMP_DIR/cat-base.sh
./$COMP_DIR/pnut-sh-optimized.exe $DIR/cat.c > $COMP_DIR/cat-optimized.sh

for len in $lengths; do
  for shell in $shells; do
    with_size "$shell" $len
  done
done
