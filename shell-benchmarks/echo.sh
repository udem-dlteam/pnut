#! /bin/sh
# Evaluate the impact of quoting arguments to functions.

if [ $# -lt 1 ]; then
  set ksh dash bash zsh
fi

while [ $# -gt 0 ]; do
  echo "\n$1: echo without quotes"
  time $1 -c 'c="abc"; i=0 ; while [ $i -lt 10000 ]; do i=$((i+1)); echo $c; done > /dev/null'

  echo "\n$1: echo with quotes"
  time $1 -c 'c="abc"; i=0 ; while [ $i -lt 10000 ]; do i=$((i+1)); echo "$c"; done > /dev/null'

  echo "\n$1: sum without quotes"
  time $1 -c 'fun() { return $#; }; acc=0; c="abc"; i=0 ; while [ $i -lt 10000 ]; do i=$((i+1)); fun $acc; done'

  echo "\n$1: sum with quotes"
  time $1 -c 'fun() { return $#; }; acc=0; c="abc"; i=0 ; while [ $i -lt 10000 ]; do i=$((i+1)); fun "$acc"; done'
  shift
done
