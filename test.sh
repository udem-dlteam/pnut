#!/bin/ksh
var() { : $((SP = SP+1)) $((_$SP = $1)) $(($1 = $2)) ; }
unvar() { : $(($1 = _$SP)) $((SP = SP-1)) ; }
_putchar() { printf \\$(($1/64))$(($1/8%8))$(($1%8)) ; }
_main() {
  var n 31416
  var p 1
  while [ 0 != $(( p * 10 <= n )) ] ; do
    : $(( p *= 10 ))
  done
  while [ 0 != $(( p > 0 )) ] ; do
    var digit $(( n / p ))
    _putchar $(( 48 + digit ))
    : $(( n %= p ))
    : $(( p /= 10 ))
    unvar digit
  done
  _putchar 10
  unvar p ; unvar n
}
_main
