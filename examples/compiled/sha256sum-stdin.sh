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

defarr _k 64
_sha256_setup() {
  : $((_$((_k + 0)) = 0x428a2f98))
  : $((_$((_k + 1)) = 0x71374491))
  : $((_$((_k + 2)) = 0xb5c0fbcf))
  : $((_$((_k + 3)) = 0xe9b5dba5))
  : $((_$((_k + 4)) = 0x3956c25b))
  : $((_$((_k + 5)) = 0x59f111f1))
  : $((_$((_k + 6)) = 0x923f82a4))
  : $((_$((_k + 7)) = 0xab1c5ed5))
  : $((_$((_k + 8)) = 0xd807aa98))
  : $((_$((_k + 9)) = 0x12835b01))
  : $((_$((_k + 10)) = 0x243185be))
  : $((_$((_k + 11)) = 0x550c7dc3))
  : $((_$((_k + 12)) = 0x72be5d74))
  : $((_$((_k + 13)) = 0x80deb1fe))
  : $((_$((_k + 14)) = 0x9bdc06a7))
  : $((_$((_k + 15)) = 0xc19bf174))
  : $((_$((_k + 16)) = 0xe49b69c1))
  : $((_$((_k + 17)) = 0xefbe4786))
  : $((_$((_k + 18)) = 0xfc19dc6))
  : $((_$((_k + 19)) = 0x240ca1cc))
  : $((_$((_k + 20)) = 0x2de92c6f))
  : $((_$((_k + 21)) = 0x4a7484aa))
  : $((_$((_k + 22)) = 0x5cb0a9dc))
  : $((_$((_k + 23)) = 0x76f988da))
  : $((_$((_k + 24)) = 0x983e5152))
  : $((_$((_k + 25)) = 0xa831c66d))
  : $((_$((_k + 26)) = 0xb00327c8))
  : $((_$((_k + 27)) = 0xbf597fc7))
  : $((_$((_k + 28)) = 0xc6e00bf3))
  : $((_$((_k + 29)) = 0xd5a79147))
  : $((_$((_k + 30)) = 0x6ca6351))
  : $((_$((_k + 31)) = 0x14292967))
  : $((_$((_k + 32)) = 0x27b70a85))
  : $((_$((_k + 33)) = 0x2e1b2138))
  : $((_$((_k + 34)) = 0x4d2c6dfc))
  : $((_$((_k + 35)) = 0x53380d13))
  : $((_$((_k + 36)) = 0x650a7354))
  : $((_$((_k + 37)) = 0x766a0abb))
  : $((_$((_k + 38)) = 0x81c2c92e))
  : $((_$((_k + 39)) = 0x92722c85))
  : $((_$((_k + 40)) = 0xa2bfe8a1))
  : $((_$((_k + 41)) = 0xa81a664b))
  : $((_$((_k + 42)) = 0xc24b8b70))
  : $((_$((_k + 43)) = 0xc76c51a3))
  : $((_$((_k + 44)) = 0xd192e819))
  : $((_$((_k + 45)) = 0xd6990624))
  : $((_$((_k + 46)) = 0xf40e3585))
  : $((_$((_k + 47)) = 0x106aa070))
  : $((_$((_k + 48)) = 0x19a4c116))
  : $((_$((_k + 49)) = 0x1e376c08))
  : $((_$((_k + 50)) = 0x2748774c))
  : $((_$((_k + 51)) = 0x34b0bcb5))
  : $((_$((_k + 52)) = 0x391c0cb3))
  : $((_$((_k + 53)) = 0x4ed8aa4a))
  : $((_$((_k + 54)) = 0x5b9cca4f))
  : $((_$((_k + 55)) = 0x682e6ff3))
  : $((_$((_k + 56)) = 0x748f82ee))
  : $((_$((_k + 57)) = 0x78a5636f))
  : $((_$((_k + 58)) = 0x84c87814))
  : $((_$((_k + 59)) = 0x8cc70208))
  : $((_$((_k + 60)) = 0x90befffa))
  : $((_$((_k + 61)) = 0xa4506ceb))
  : $((_$((_k + 62)) = 0xbef9a3f7))
  : $((_$((_k + 63)) = 0xc67178f2))
}

defarr _w 64
_nbits=0
defarr _hash 8
defarr _temp 8
_sha256_init() {
  _nbits=0
  : $((_$((_hash + 0)) = 0x6a09e667))
  : $((_$((_hash + 1)) = 0xbb67ae85))
  : $((_$((_hash + 2)) = 0x3c6ef372))
  : $((_$((_hash + 3)) = 0xa54ff53a))
  : $((_$((_hash + 4)) = 0x510e527f))
  : $((_$((_hash + 5)) = 0x9b05688c))
  : $((_$((_hash + 6)) = 0x1f83d9ab))
  : $((_$((_hash + 7)) = 0x5be0cd19))
}

: $((t2 = ma = t1 = ch = i = s1 = s0 = b3 = b2 = b1 = b0 = bytes = 0))
_sha256_add_block() { let bytes $2
  let b0; let b1; let b2; let b3; let s0; let s1; let i; let ch; let t1; let ma; let t2
  i=0
  while [ $i -lt 16 ]; do
    b0=$((0xff & _$((bytes + (i * 4)))))
    b1=$((0xff & _$((bytes + (i * 4) + 1))))
    b2=$((0xff & _$((bytes + (i * 4) + 2))))
    b3=$((0xff & _$((bytes + (i * 4) + 3))))
    : $((_$((_w + i)) = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3))
    : $((i += 1))
  done
  i=16
  while [ $i -lt 64 ]; do
    s0=$(((((_$((_w + (i - 15))) >> 7) & (0x7fffffff >> (7 - 1))) | ((_$((_w + (i - 15))) << (32 - 7)) & 0xffffffff)) ^ (((_$((_w + (i - 15))) >> 18) & (0x7fffffff >> (18 - 1))) | ((_$((_w + (i - 15))) << (32 - 18)) & 0xffffffff)) ^ ((_$((_w + (i - 15))) >> 3) & 0x1fffffff)))
    s1=$(((((_$((_w + (i - 2))) >> 17) & (0x7fffffff >> (17 - 1))) | ((_$((_w + (i - 2))) << (32 - 17)) & 0xffffffff)) ^ (((_$((_w + (i - 2))) >> 19) & (0x7fffffff >> (19 - 1))) | ((_$((_w + (i - 2))) << (32 - 19)) & 0xffffffff)) ^ ((_$((_w + (i - 2))) >> 10) & 0x3fffff)))
    : $((_$((_w + i)) = (_$((_w + (i - 16))) + s0 + _$((_w + (i - 7))) + s1) & 0xffffffff))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 8 ]; do
    : $((_$((_temp + i)) = _$((_hash + i))))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 64 ]; do
    s1=$(((((_$((_temp + 4)) >> 6) & (0x7fffffff >> (6 - 1))) | ((_$((_temp + 4)) << (32 - 6)) & 0xffffffff)) ^ (((_$((_temp + 4)) >> 11) & (0x7fffffff >> (11 - 1))) | ((_$((_temp + 4)) << (32 - 11)) & 0xffffffff)) ^ (((_$((_temp + 4)) >> 25) & (0x7fffffff >> (25 - 1))) | ((_$((_temp + 4)) << (32 - 25)) & 0xffffffff))))
    ch=$(((_$((_temp + 4)) & _$((_temp + 5))) ^ (~_$((_temp + 4)) & _$((_temp + 6)))))
    t1=$(((_$((_temp + 7)) + s1 + ch + _$((_k + i)) + _$((_w + i))) & 0xffffffff))
    s0=$(((((_$((_temp + 0)) >> 2) & (0x7fffffff >> (2 - 1))) | ((_$((_temp + 0)) << (32 - 2)) & 0xffffffff)) ^ (((_$((_temp + 0)) >> 13) & (0x7fffffff >> (13 - 1))) | ((_$((_temp + 0)) << (32 - 13)) & 0xffffffff)) ^ (((_$((_temp + 0)) >> 22) & (0x7fffffff >> (22 - 1))) | ((_$((_temp + 0)) << (32 - 22)) & 0xffffffff))))
    ma=$(((_$((_temp + 0)) & _$((_temp + 1))) ^ (_$((_temp + 0)) & _$((_temp + 2))) ^ (_$((_temp + 1)) & _$((_temp + 2)))))
    t2=$(((s0 + ma) & 0xffffffff))
    : $((_$((_temp + 7)) = _$((_temp + 6))))
    : $((_$((_temp + 6)) = _$((_temp + 5))))
    : $((_$((_temp + 5)) = _$((_temp + 4))))
    : $((_$((_temp + 4)) = (_$((_temp + 3)) + t1) & 0xffffffff))
    : $((_$((_temp + 3)) = _$((_temp + 2))))
    : $((_$((_temp + 2)) = _$((_temp + 1))))
    : $((_$((_temp + 1)) = _$((_temp + 0))))
    : $((_$((_temp + 0)) = (t1 + t2) & 0xffffffff))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 8 ]; do
    : $((_$((_hash + i)) = (_$((_hash + i)) + _$((_temp + i))) & 0xffffffff))
    : $((i += 1))
  done
  endlet $1 t2 ma t1 ch i s1 s0 b3 b2 b1 b0 bytes
}

defarr _buf 64
_hex_nibble() { # byte: $2
  if [ $(($2 & 0xf)) -lt 10 ] ; then
    : $(($1 = __0__ + ($2 & 0xf)))
  else
    : $(($1 = __a__ + (($2 & 0xf) - 10)))
  fi
}

: $((__t1 = 0))
_hex() { # byte: $2
  let __t1
  _hex_nibble __t1 $(($2 >> 4))
  printf \\$(((__t1)/64))$(((__t1)/8%8))$(((__t1)%8))
  _hex_nibble __t1 $2
  printf \\$(((__t1)/64))$(((__t1)/8%8))$(((__t1)%8))
  endlet $1 __t1
}

: $((c = n = size = buffer = 0))
_read_stdin() { let buffer $2; let size $3
  let n; let c
  while [ $n -lt $size ]; do
    _getchar c
    if [ $c = $_EOF ] ; then
      break
    fi
    : $((_$((buffer + n)) = c))
    : $(((n += 1) - 1))
  done
  : $(($1 = n))
  endlet $1 c n size buffer
}

: $((h = n = i = 0))
_process_stdin() {
  let i; let n; let h
  n=64
  _sha256_setup __
  _sha256_init __
  while [ $n = 64 ]; do
    _read_stdin n $_buf 64
    if [ $n -lt 0 ] ; then
      : $(($1 = 1))
      endlet $1 h n i
      return
    fi
    : $((_nbits += (8 * n)))
    if [ $n -lt 64 ] ; then
      : $((_$((_buf + n)) = 0x80))
      i=$((n + 1))
      while [ $i -lt 64 ]; do
        : $((_$((_buf + i)) = 0))
        : $((i += 1))
      done
      if [ $n -ge $((64 - 9)) ] ; then
        _sha256_add_block __ $_buf
        i=0
        while [ $i -lt $((64 - 8)) ]; do
          : $((_$((_buf + i)) = 0))
          : $((i += 1))
        done
      fi
      i=1
      while [ $i -le 8 ]; do
        : $((_$((_buf + 64 - i)) = 0xff & _nbits))
        : $((_nbits >>= 8))
        : $((i += 1))
      done
    fi
    _sha256_add_block __ $_buf
  done
  i=0
  while [ $i -lt 8 ]; do
    h=$((_$((_hash + i))))
    _hex __ $((h >> 24))
    _hex __ $((h >> 16))
    _hex __ $((h >> 8))
    _hex __ $h
    : $((i += 1))
  done
  printf " "
  printf " "
  while [ $((_$_filename)) != 0 ]; do
    printf \\$(((_$_filename)/64))$(((_$_filename)/8%8))$(((_$_filename)%8))
    : $((_filename += 1))
  done
  printf "\n"
  : $(($1 = 0))
  endlet $1 h n i
}

_main() {
  _process_stdin __
  : $(($1 = 0))
}

#_ Character constants
readonly __0__=48
readonly __a__=97
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


__stdin_buf=
__stdin_line_ending=0 # Line ending, either -1 (EOF) or 10 ('\n')
_getchar() {
  if [ -z "$__stdin_buf" ]; then          # need to get next line when buffer empty
    if [ $__stdin_line_ending != 0 ]; then  # Line is empty, return line ending
      : $(($1 = __stdin_line_ending))
      __stdin_line_ending=0                  # Reset line ending for next getchar call
      return
    fi
    IFS=                                            # don't split input
    if read -r __stdin_buf ; then                   # read next line into $__stdin_buf
      if [ -z "$__stdin_buf" ] ; then               # an empty line implies a newline character
        : $(($1 = 10))                              # next getchar call will read next line
        return
      fi
      __stdin_line_ending=10
    else
      if [ -z "$__stdin_buf" ] ; then               # EOF reached when read fails
        : $(($1 = -1))
        return
      else
        __stdin_line_ending=-1
      fi
    fi
  fi
  __c=$(printf "%d" "'${__stdin_buf%"${__stdin_buf#?}"}"); __c=$((__c > 0 ? __c : 256 + __c))
  : $(($1 = __c))
    __stdin_buf="${__stdin_buf#?}"                  # remove the current char from $__stdin_buf
}

__code=0; # Exit code
_main __code
exit $__code
