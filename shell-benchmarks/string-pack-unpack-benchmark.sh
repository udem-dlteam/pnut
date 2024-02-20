#! /bin/sh
# Evaluate the performance of packing and unpacking a Shell string to the heap.

if [ $# -lt 1 ]; then
  set ksh dash bash zsh
fi

while [ $# -gt 0 ]; do
  echo "$1:"
  time $1 -c 'source shell-benchmarks/string-pack-unpack-benchmark-code.sh'
  shift
done
