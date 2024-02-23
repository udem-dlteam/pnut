#! /bin/sh
# Evaluate the performance of encoding a character to an integer.
# Conclusion:
# The results vary depending of the number of variables in the environment.
# When the environment is empty, a lookup table (method 3) is faster than a case statement.
# When the environment is not empty (with 100000 variables initialized), a case statement (method 1 and 2) is faster than a lookup table.
# The difference between method 1 (long case statement) and 2 (tree-like case statements) is negligible, excepf for ksh where method 1 is faster.
# In all cases, using printf is at least 4x as slow as the other methods.

if [ $# -lt 1 ]; then
  set ksh dash bash zsh
fi

while [ $# -gt 0 ]; do
  echo "$1:"
  method=0
  while [ "$method" -lt 4 ]; do
    echo "\nMethod $method:"
    time $1 -c "shell-benchmarks/char-to-integer-code.sh $method"
    method=$((method + 1))
  done
  echo ""
  shift
done
