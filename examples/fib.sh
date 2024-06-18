set -e -u

#################################### C code ####################################
# int fib(int n) {
#   if (n < 2) {
#     return n;
#   } else {
#     return fib(n - 1) + fib(n - 2);
#   }
# }
################################# End of C code ################################
: $((__g2 = __g1 = n = 0))
_fib() { # n: $2
  let n; let __g1; let __g2
  n=$2
  if [ $n -lt 2 ] ; then
    : $(( $1 = n ))
  else
    _fib __g1 $((n - 1))
    _fib __g2 $((n - 2))
    : $(( $1 = (__g1 + __g2) ))
  fi
  endlet $1 __g2 __g1 n
}

#################################### C code ####################################
# void main() {
#   int n;
#   n = fib(20);
#   printf("fib(20) = %d\n", n);
# }
################################# End of C code ################################
: $((n = 0))
_main() {
  let n
  n=0
  _fib n 20
  printf "fib(20) = %d\n" $n
  endlet $1 n
}

# Runtime library
# Local variables
__=0
__SP=0
let() { : $((__SP += 1)) $((__$__SP=$1)); } 
endlet() {
  __ret=$1; : $((__tmp = $__ret)) # Save return value so it's not overwritten
  while [ $# -ge 2 ]; do : $(($2 = __$__SP)) $((__SP -= 1)); shift; done
  : $(($__ret=__tmp))
}

_main __

# string_pool_alloc=371 heap_alloc=551 text_alloc=16
