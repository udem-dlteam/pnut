#! /bin/sh
# Evaluate impact of fast buffer optimization

if [ $# -lt 1 ]; then
  set ksh bash zsh
  # Dash cannot read the file for some reason
fi

while [ $# -gt 0 ]; do
  echo "\n$1 fast:"
  time $1 -c '. ./shell-benchmarks/get_char_code_fast.sh 100 ./shell-benchmarks/get_char_data.txt'

  echo "\n$1 slow:"
  time $1 -c '. ./shell-benchmarks/get_char_code_slow.sh 100 ./shell-benchmarks/get_char_data.txt'
  shift
done
