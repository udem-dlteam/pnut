#!/bin/sh
set -e -u -f
LC_ALL=C

: $((__t2 = __t1 = n = 0))
_fib() { let n $2
  let __t1; let __t2
  if [ $n -lt 2 ] ; then
    : $(($1 = n))
  else
    _fib __t1 $((n - 1))
    _fib __t2 $((n - 2))
    : $(($1 = __t1 + __t2))
  fi
  endlet $1 __t2 __t1 n
}

: $((i = n = 0))
_main() {
  let n; let i
  i=0
  while [ $i -lt 20 ]; do
    _fib n $i
    printf "fib(%d) = %d\n" $i $n
    : $((i += 1))
  done
  endlet $1 i n
}

#_ Runtime library
#_ Local variables
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
