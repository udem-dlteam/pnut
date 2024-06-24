set -e -u

#################################### C code ####################################
# // print all characters
# void main() {
#   char c = 0;
#   while (c < 128) {
#     putchar(c);
#     c += 1;
#   }
# }
################################# End of C code ################################
: $((c = 0))
_main() {
  let c
  c=0
  while [ $c -lt 128 ] ; do
    printf \\$(($c/64))$(($c/8%8))$(($c%8))
    : $((c += 1))
  done
  endlet $1 c
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

# string_pool_alloc=375 heap_alloc=433 max_text_alloc=266 cumul_text_alloc=266
