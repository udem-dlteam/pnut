#! /bin/sh
# Evaluate the different ways to assign variables using arithmetic expansion.

if [ $# -lt 1 ]; then
  set ksh dash bash zsh
fi

while [ $# -gt 0 ]; do
  echo "\n$1: Assignment outside of \$(())"
  time $1 -c 'acc=""; i=0 ; while [ $i -lt 100000 ]; do i=$((i+1)); acc=$((acc + 1)); done'

  echo "\n$1: Assignment outside of \$(()) with quotes"
  time $1 -c 'acc=""; i=0 ; while [ $i -lt 100000 ]; do i=$((i+1)); acc=$"((acc + 1))"; done'

  echo "\n$1: Assignment in \$(())"
  time $1 -c 'acc=""; i=0 ; while [ $i -lt 100000 ]; do i=$((i+1)); : $((acc = acc + 1)); done'
  shift
done
