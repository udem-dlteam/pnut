#! /bin/sh
# Evaluate the time to create dynamic variables.
# Some shells use a hash table to store variables, and others a linked list, this script will help categorize them.
# Shells with hash table will have a NlogN relationship between time and the number of variables (N)
# Shells with linked list will have a N^2 relationship between time and the number of variables (N)

if [ $# -lt 1 ]; then
  set ksh dash bash zsh
fi

while [ $# -gt 0 ]; do
  echo "\n$1: 10000 variables"
  time $1 -c 'i=0 ; while [ $i -lt 10000 ]; do i=$((i+1)); : $((acc_$i = 1)); done'

  echo "\n$1: 100000 variables"
  time $1 -c 'i=0 ; while [ $i -lt 100000 ]; do i=$((i+1)); : $((acc_$i = 1)); done'

  echo "\n$1: 500000 variables"
  time $1 -c 'i=0 ; while [ $i -lt 500000 ]; do i=$((i+1)); : $((acc_$i = 1)); done'
  shift
done
