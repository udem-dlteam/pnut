#! /bin/sh
# Evaluate impact of fast buffer optimization

if [ $# -lt 1 ]; then
  set ksh dash bash zsh
  # Dash cannot read the file for some reason
fi

while [ $# -gt 0 ]; do
  echo "\n$1 fast (empty env):"
  $1 -c 'shell-benchmarks/get_char_code_fast.sh 0 24000 ./shell-benchmarks/get_char_data.txt'

  echo "\n$1 fast (10k env):"
  $1 -c 'shell-benchmarks/get_char_code_fast.sh 10000 24000 ./shell-benchmarks/get_char_data.txt'

  echo "\n$1 fast (100k env):"
  $1 -c 'shell-benchmarks/get_char_code_fast.sh 100000 24000 ./shell-benchmarks/get_char_data.txt'

  echo "\n$1 fast (500k env):"
  $1 -c 'shell-benchmarks/get_char_code_fast.sh 500000 24000 ./shell-benchmarks/get_char_data.txt'

  echo "\n$1 slow (empty env):"
  $1 -c 'shell-benchmarks/get_char_code_slow.sh 0 24000 ./shell-benchmarks/get_char_data.txt'

  echo "\n$1 slow (10k env):"
  $1 -c 'shell-benchmarks/get_char_code_slow.sh 10000 24000 ./shell-benchmarks/get_char_data.txt'

  echo "\n$1 slow (100k env):"
  $1 -c 'shell-benchmarks/get_char_code_slow.sh 100000 24000 ./shell-benchmarks/get_char_data.txt'

  echo "\n$1 slow (500k env):"
  $1 -c 'shell-benchmarks/get_char_code_slow.sh 500000 24000 ./shell-benchmarks/get_char_data.txt'
  shift
done
