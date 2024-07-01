#!/bin/bash
set -e

DIR="benchmarks/sha256sum"
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
  for len in $lengths; do
    base64 /dev/urandom | head -c $len > "$INPUT_DIR/input-$len.txt"
  done
}

with_size() {
  shell=$1
  len=$2

  TIME_MS=$(( $( { time $shell $COMP_DIR/sha256sum-base.sh $INPUT_DIR/input-$len.txt; } 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g" ) ))
  print_time $TIME_MS "for: $shell with FILESIZE=$len and base"

  TIME_MS=$(( $( { time $shell $COMP_DIR/sha256sum-optimized.sh $INPUT_DIR/input-$len.txt; } 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g" ) ))
  print_time $TIME_MS "for: $shell with FILESIZE=$len and optimized"
}

lengths="64 128 256 512 1024"
shells="dash bash yash zsh ksh"

# Generate input files
generate_input_files

# Compile pnut with
PNUT_OPTIONS="-DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Dsh"

for BLOCK_SIZE in $lengths; do
  gcc -o $COMP_DIR/pnut-sh-base-$BLOCK_SIZE.exe $PNUT_OPTIONS -DBLOCK_SIZE=$BLOCK_SIZE pnut.c
  gcc -o $COMP_DIR/pnut-sh-optimized-$BLOCK_SIZE.exe $PNUT_OPTIONS -DOPTIMIZE_LONG_LINES -DBLOCK_SIZE=$BLOCK_SIZE pnut.c

  ./$COMP_DIR/pnut-sh-base-$BLOCK_SIZE.exe $DIR/sha256sum.c > $COMP_DIR/sha256sum-base.sh
  ./$COMP_DIR/pnut-sh-optimized-$BLOCK_SIZE.exe $DIR/sha256sum.c > $COMP_DIR/sha256sum-optimized.sh

  for shell in $shells; do
    with_size "$shell" $BLOCK_SIZE
  done
done
