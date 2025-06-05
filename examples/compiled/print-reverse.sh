#!/bin/sh
set -e -u -f
LC_ALL=C

: $((i = len = tmp = end = str = 0))
_reverse_str() { let str $2
  let end; let tmp; let len; let i
  end=$str
  i=0
  while [ $((_$(((end += 1) - 1)))) != 0 ]; do
    :
  done
  len=$(((end - str) - 1))
  while [ $i -lt $((len / 2)) ]; do
    tmp=$((_$((str + i))))
    : $((_$((str + i)) = _$((str + ((len - 1) - i)))))
    : $((_$((str + (len - 1) - i)) = tmp))
    : $((i += 1))
  done
  endlet $1 i len tmp end str
}

: $((i = argv_ = argc = 0))
_main() { let argc $2; let argv_ $3
  let i
  i=1
  while [ $i -lt $argc ]; do
    _reverse_str __ $((_$((argv_ + i))))
    _put_pstr __ $i
    printf "\n"
    : $((i += 1))
  done
  endlet $1 i argv_ argc
}

# Runtime library
_put_pstr() {
  : $(($1 = 0)); shift # Return 0
  __addr=$1; shift
  while [ $((__c = _$__addr)) != 0 ]; do
    printf \\$((__c/64))$((__c/8%8))$((__c%8))
    : $((__addr += 1))
  done
}

__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $(($1 = $__ALLOC + 1))
  : $((__ALLOC += $2 + 1))
}


# Unpack a Shell string into an appropriately sized buffer
unpack_string() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?
  __fgetc_buf=$1
  __buffer=$2
  __ends_with_eof=$3
  while [ ! -z "$__fgetc_buf" ]; do
    __c=$(printf "%d" "'${__fgetc_buf%"${__fgetc_buf#?}"}"); __c=$((__c > 0 ? __c : 256 + __c))
    : $((_$__buffer = __c))
    __fgetc_buf=${__fgetc_buf#?}      # Remove the first character
    : $((__buffer += 1))              # Move to the next buffer position
  done

  if [ $__ends_with_eof -eq 0 ]; then # Ends with newline and not EOF?
    : $((_$__buffer = 10))            # Line ends with newline
    : $((__buffer += 1))
  fi
  : $((_$__buffer = 0))               # Then \0
}

make_argv() {
  __argc=$1; shift;
  _malloc __argv $__argc # Allocate enough space for all elements. No need to initialize.
  __argv_ptr=$__argv

  while [ $# -ge 1 ]; do
    _malloc _$__argv_ptr $((${#1} + 1))
    unpack_string "$1" $((_$__argv_ptr)) 1
    : $((__argv_ptr += 1))
    shift
  done
}

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
make_argv $(($# + 1)) "$0" "$@" # Setup argc/argv
_main __code $(($# + 1)) $__argv
exit $__code
