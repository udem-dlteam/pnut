#! /bin/sh
# Evaluate impact of fast buffer optimization

if [ $# -lt 1 ]; then
  set ksh dash bash zsh
fi

while [ $# -gt 0 ]; do
  echo "\n$1: Without quotes"
  time $1 -c 'acc=""; c="abc"; i=0 ; while [ $i -lt 10000 ]; do i=$((i+1)); done'

  echo "\n$1: With quotes"
  time $1 -c 'acc=""; c="abc"; i=0 ; while [ "$i" -lt 10000 ]; do i=$((i+1)); done'
  shift
done
