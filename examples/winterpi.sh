set -e -u

# /* https://cs.uwaterloo.ca/~alopez-o/math-faq/mathtext/node12.html */
# int dummy = 0;
_dummy=0
#################################### C code ####################################
# int main() {
#   int r[2801];
#   int i;
#   int k;
#   int b;
#   int d;
#   int c = 0;
#   i = 0;
#   while (i < 2800) {
#     r[i] = 2000;
#     i = i+1;
#   }
#   r[i] = 0;
#   k = 2800;
#   while (k > 0) {
#     d = 0;
#     i = k;
#     while (i > 0) {
#       d = d * i;
#       d = d + r[i] * 10000;
#       b = 2 * i - 1;
#       r[i] = d % b;
#       d = d / b;
#       i = i-1;
#     }
#     putchar(48 + (c + d / 10000) / 1000 % 10);
#     putchar(48 + (c + d / 10000) / 100 % 10);
#     putchar(48 + (c + d / 10000) / 10 % 10);
#     putchar(48 + (c + d / 10000) % 10);
#     c = d % 10000;
#     k = k - 14;
#   }
#   putchar(10);
#   return 0;
# }
################################# End of C code ################################
: $((c = d = b = k = i = r = 0))
_main() {
  let r; let i; let k; let b; let d; let c
  r=0
  i=0
  k=0
  b=0
  d=0
  c=0
  i=0
  while [ $i -lt 2800 ] ; do
    : $(( _$((r+i)) = 2000 ))
    i=$((i + 1))
  done
  : $(( _$((r+i)) = 0 ))
  k=2800
  while [ $k -gt 0 ] ; do
    d=0
    i=$k
    while [ $i -gt 0 ] ; do
      d=$((d * i))
      d=$((d + (_$((r+i)) * 10000)))
      b=$(((2 * i) - 1))
      : $(( _$((r+i)) = (d % b) ))
      d=$((d / b))
      i=$((i - 1))
    done
    printf \\$(($((48 + (((c + (d / 10000)) / 1000) % 10)))/64))$(($((48 + (((c + (d / 10000)) / 1000) % 10)))/8%8))$(($((48 + (((c + (d / 10000)) / 1000) % 10)))%8))
    printf \\$(($((48 + (((c + (d / 10000)) / 100) % 10)))/64))$(($((48 + (((c + (d / 10000)) / 100) % 10)))/8%8))$(($((48 + (((c + (d / 10000)) / 100) % 10)))%8))
    printf \\$(($((48 + (((c + (d / 10000)) / 10) % 10)))/64))$(($((48 + (((c + (d / 10000)) / 10) % 10)))/8%8))$(($((48 + (((c + (d / 10000)) / 10) % 10)))%8))
    printf \\$(($((48 + ((c + (d / 10000)) % 10)))/64))$(($((48 + ((c + (d / 10000)) % 10)))/8%8))$(($((48 + ((c + (d / 10000)) % 10)))%8))
    c=$((d % 10000))
    k=$((k - 14))
  done
  printf \\$((10/64))$((10/8%8))$((10%8))
  : $(( $1 = 0 ))
  endlet $1 c d b k i r
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

_code=0; # Success exit code
_main _code; exit $_code

# string_pool_alloc=369 heap_alloc=1139 text_alloc=35
