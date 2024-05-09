#! /bin/sh
# Evaluate the impact of name length on performance.

# if [ $# -lt 1 ]; then
#   set ksh dash bash zsh
# fi

# while [ $# -gt 0 ]; do
#   echo "\n$1: 10000 variables"
#   time $1 -c 'i=0 ; while [ $i -lt 10000 ]; do i=$((i+1)); : $((acc_$i = 1)); done'

#   echo "\n$1: 100000 variables"
#   time $1 -c 'i=0 ; while [ $i -lt 100000 ]; do i=$((i+1)); : $((acc_$i = 1)); done'

#   echo "\n$1: 500000 variables"
#   time $1 -c 'i=0 ; while [ $i -lt 500000 ]; do i=$((i+1)); : $((acc_$i = 1)); done'
#   shift
# done

SIZE=100000
HEAP=52233720368547
PREFIX=__________

alloc_array() {
  i=0
  while [ $i -lt $SIZE ]; do
    : $((_$((HEAP + i)) = i))
    i=$((i+1))
  done
}

sum_array() {
  i=0
  sum=0
  while [ $i -lt $SIZE ]; do
    i=$((i+1))
    : $((sum += _$((HEAP + i))))
  done
  # echo $sum
}

alloc_array
sum_array
