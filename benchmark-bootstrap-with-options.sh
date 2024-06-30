DIR="benchmarks/long-lines"
COMP_DIR="$DIR/compiled"

# Create the compiled directory if it doesn't exist
mkdir -p $COMP_DIR

print_time()
{
  ms=$1
  printf "%s %s\n" "$((ms/1000)).$((ms/100%10))$((ms/10%10))$((ms%10))s" "$2"
}

shells="ksh dash bash yash zsh"

with_options() {
  for shell in $shells; do
    ./benchmark-bootstrap.sh $shell $@
  done
}

# Bootstrap benchmarks:
#   - Baseline
#   - With set
#   - Including C code
#   - Inlined characters
#   - Optimize long lines

with_options
with_options "-DSH_SAVE_VARS_WITH_SET"
with_options "-DSH_INCLUDE_C_CODE"
with_options "-DSH_INLINE_CHAR_LITERAL"
with_options "-DOPTIMIZE_LONG_LINES"
