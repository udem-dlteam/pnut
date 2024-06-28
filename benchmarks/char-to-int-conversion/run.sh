DIR="benchmarks/char-to-int-conversion"

print_time()
{
  ms=$1
  printf "%s %s\n" "$((ms/1000)).$((ms/100%10))$((ms/10%10))$((ms%10))s" "$2"
}

with_method() {
  method=$2

  TIME_MS=$(( `bash -c "time $1 $DIR/$method.sh 0 100" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $TIME_MS "for: $SHELL_TO_TEST $method.sh (empty env)"

  TIME_MS_INIT=$(( `bash -c "time $1 $DIR/$method.sh 1000 0" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  TIME_MS=$(( `bash -c "time $1 $DIR/$method.sh 1000 100" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $((TIME_MS - TIME_MS_INIT)) "for: $SHELL_TO_TEST $method.sh (1000 vars)"

  TIME_MS_INIT=$(( `bash -c "time $1 $DIR/$method.sh 10000 0" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  TIME_MS=$(( `bash -c "time $1 $DIR/$method.sh 10000 100" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $((TIME_MS - TIME_MS_INIT)) "for: $SHELL_TO_TEST $method.sh (10000 vars)"

  TIME_MS_INIT=$(( `bash -c "time $1 $DIR/$method.sh 100000 0" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  TIME_MS=$(( `bash -c "time $1 $DIR/$method.sh 100000 100" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $((TIME_MS - TIME_MS_INIT)) "for: $SHELL_TO_TEST $method.sh (100000 vars)"

  TIME_MS_INIT=$(( `bash -c "time $1 $DIR/$method.sh 500000 0" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  TIME_MS=$(( `bash -c "time $1 $DIR/$method.sh 500000 100" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  print_time $((TIME_MS - TIME_MS_INIT)) "for: $SHELL_TO_TEST $method.sh (500000 vars)"
}

sizes="subshell case lookup_table"
shells="ksh dash bash yash zsh"

for shell in $shells; do
  printf "With $shell\n"
  for size in $sizes; do
    with_method "$shell" $size
  done
done
