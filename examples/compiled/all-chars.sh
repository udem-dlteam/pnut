#!/bin/sh
set -e -u -f
LC_ALL=C

: $((c = 0))
_main() {
  let c
  c=0
  while [ $c -lt 128 ] ; do
    printf \\$((c/64))$((c/8%8))$((c%8))
    : $((c += 1))
  done
  endlet $1 c
}

# Runtime library
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
