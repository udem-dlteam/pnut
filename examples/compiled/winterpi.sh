#!/bin/sh
set -e -u -f
LC_ALL=C

__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $(($1 = $__ALLOC + 1))
  : $((__ALLOC += $2 + 1))
}

defarr() { _malloc $1 $2; }

defarr _r 2801
: $((c = d = b = k = i = 0))
_main() {
  let i; let k; let b; let d; let c
  c=0
  i=0
  while [ $i -lt 2800 ]; do
    : $((_$((_r + i)) = 2000))
    i=$((i + 1))
  done
  : $((_$((_r + i)) = 0))
  k=2800
  while [ $k -gt 0 ]; do
    d=0
    i=$k
    while [ $i -gt 0 ]; do
      d=$((d * i))
      d=$((d + (_$((_r + i)) * 10000)))
      b=$(((2 * i) - 1))
      : $((_$((_r + i)) = d % b))
      d=$((d / b))
      i=$((i - 1))
    done
    printf \\$(((__0__ + (((c + (d / 10000)) / 1000) % 10))/64))$(((__0__ + (((c + (d / 10000)) / 1000) % 10))/8%8))$(((__0__ + (((c + (d / 10000)) / 1000) % 10))%8))
    printf \\$(((__0__ + (((c + (d / 10000)) / 100) % 10))/64))$(((__0__ + (((c + (d / 10000)) / 100) % 10))/8%8))$(((__0__ + (((c + (d / 10000)) / 100) % 10))%8))
    printf \\$(((__0__ + (((c + (d / 10000)) / 10) % 10))/64))$(((__0__ + (((c + (d / 10000)) / 10) % 10))/8%8))$(((__0__ + (((c + (d / 10000)) / 10) % 10))%8))
    printf \\$(((__0__ + ((c + (d / 10000)) % 10))/64))$(((__0__ + ((c + (d / 10000)) % 10))/8%8))$(((__0__ + ((c + (d / 10000)) % 10))%8))
    c=$((d % 10000))
    k=$((k - 14))
  done
  printf \\$(((__NEWLINE__)/64))$(((__NEWLINE__)/8%8))$(((__NEWLINE__)%8))
  : $(($1 = 0))
  endlet $1 c d b k i
}

# Character constants
readonly __NEWLINE__=10
readonly __0__=48
# Runtime library
# Local variables
__=0
__SP=0
let() { # $1: variable name, $2: value (optional)
  : $((__$((__SP += 1))=$1)) # Push
  : $(($1=${2-0}))           # Init
}
endlet() { # $1: return variable
           # $2...: function local variables
  __ret=$1 # Don't overwrite return value
  : $((__tmp = $__ret))
  while [ $# -ge 2 ]; do
    : $(($2 = __$(((__SP -= 1) + 1)))) # Pop
    shift;
  done
  : $(($__ret=__tmp))   # Restore return value
}

__code=0; # Exit code
_main __code
exit $__code
