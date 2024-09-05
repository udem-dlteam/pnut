#!/bin/sh
set -e -u -f
LC_ALL=C

: $((sum = i = size = n = 0))
_sum_array() { let n $2; let size $3
  let i; let sum
  i=0
  sum=0
  while [ $i -lt $size ] ; do
    : $((sum += _$((n + i))))
    : $(((i += 1) - 1))
  done
  : $(($1 = sum))
  endlet $1 sum i size n
}

: $((sum = i = n = 0))
_main() {
  let n; let i; let sum
  _malloc n 10000
  i=0
  sum=0
  while [ $i -lt 10000 ] ; do
    : $((_$((n + i)) = i))
    : $(((i += 1) - 1))
  done
  i=0
  while [ $i -lt 10000 ] ; do
    : $((sum += _$((n + i))))
    : $(((i += 1) - 1))
  done
  endlet $1 sum i n
}

# Runtime library
__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $(($1 = $__ALLOC + 1))
  : $((__ALLOC += $2 + 1))
}

# Local variables
__=0
__SP=0
let() { # $1: variable name, $2: value (optional)
  : $((__SP += 1)) $((__$__SP=$1)) # Push
  : $(($1=${2-0}))                 # Init
}
endlet() { # $1: return variable
           # $2...: function local variables
  __ret=$1 # Don't overwrite return value
  : $((__tmp = $__ret))
  while [ $# -ge 2 ]; do
    : $(($2 = __$__SP)) $((__SP -= 1)); # Pop
    shift;
  done
  : $(($__ret=__tmp))   # Restore return value
}

_main __
