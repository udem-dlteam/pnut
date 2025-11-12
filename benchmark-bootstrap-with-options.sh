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
with_options "-DSH_OPTIMIZE_LONG_LINES"
