#! /bin/sh
# Evaluate the impact of quoting arguments in variable assignment.

if [ $# -lt 1 ]; then
  set ksh dash bash zsh
fi

while [ $# -gt 0 ]; do
  echo "\n$1: Without quotes"
  time $1 -c 'acc=""; c="abc"; i=0 ; while [ $i -lt 100000 ]; do i=$((i+1)); acc=$c; done'

  echo "\n$1: With quotes"
  time $1 -c 'acc=""; c="abc"; i=0 ; while [ $i -lt 100000 ]; do i=$((i+1)); acc="$c"; done'
  shift
done
