set -e -u

# Handle runtime options
__STRICT_MODE=1
__FREE_UNSETS_VARS=1
__INIT_GLOBALS=0

if [ $# -gt 0 ] && [ $1 = "--malloc-init" ] ;      then __STRICT_MODE=1; shift; fi
if [ $# -gt 0 ] && [ $1 = "--malloc-no-init" ] ;   then __STRICT_MODE=0; shift; fi
if [ $# -gt 0 ] && [ $1 = "--free-unsets-vars" ] ; then __FREE_UNSETS_VARS=1; shift; fi
if [ $# -gt 0 ] && [ $1 = "--free-noop" ] ;        then __FREE_UNSETS_VARS=0; shift; fi
if [ $# -gt 0 ] && [ $1 = "--zero-globals" ] ;     then __INIT_GLOBALS=1; shift; fi
if [ $# -gt 0 ] && [ $1 = "--no-zero-globals" ] ;  then __INIT_GLOBALS=0; shift; fi

# Load runtime library and primitives
. ../../runtime.sh

# Local variables

__SP=0 # Note: Stack grows up, not down

save_loc_var() {
  __count=$#
  while [ $# -gt 0 ]; do
    : $((__SP += 1))
    : $((save_loc_var_$__SP=$1))
    shift
  done
  # Save the number of arguments. Used in rest_loc_var
  : $((__SP += 1))
  : $((save_loc_var_$__SP=$__count))
}

rest_loc_var() {
  __result_loc=$1; shift
  __adjust=$((save_loc_var_$__SP - $#)) # Number of saved variables not being restored
  : $((__SP -= 1))
  while [ $# -gt 0 ]; do
    # Make sure result_loc is not overwritten
    if [ $1 != "$__result_loc" ]; then : $(($1=save_loc_var_$__SP)); fi
    : $((__SP -= 1))
    shift
  done
  : $((__SP -= __adjust))
}


defarr() { alloc $2; : $(( $1 = __addr )) ; if [ $__INIT_GLOBALS -ne 0 ]; then initialize_memory $(($1)) $2; fi; }
defglo() { : $(($1 = $2)) ; }

# Setup argc, argv
__argc_for_main=$(($# + 1))
make_argv $__argc_for_main "$0" $@; __argv_for_main=$__argv

# Initialize local vars so set -u can be used
: $(( n3 = s = max = ix = sum = arr = n2 = i = len = n1 = start = 0 ))
# Heap initialization
unpack_array 1 2 3 4 5 0; __0=$__addr


_string_len() { # s: $2
  save_loc_var ix
  : $(( ix = 0 ))
  : $(( ix = 0 ))
  while [ $((_$(($2+ix)))) -ne 0 ] ; do
    : $(((ix += 1) - 1))
  done
  : $(( $1 = ix ))
  rest_loc_var $1 ix
}
_string_sum() { # s: $2
  save_loc_var sum s
  : $(( s = $2 ))
  : $(( sum = 0 ))
  : $(( sum = 0 ))
  while [ $((_$s)) -ne 0 ] ; do
    : $((sum += _$s))
    : $(((s += 1) - 1))
  done
  : $(( $1 = sum ))
  rest_loc_var $1 s sum
}
_iota_array() { # start: $2, max: $3
  save_loc_var arr i
  : $(( i = 0 ))
  : $(( arr = 0 ))
  _malloc arr $(($3 - $2))
  : $(( i = 0 ))
  while [ $((i + $2)) -lt $3 ] ; do
    : $(( _$((arr+i)) = (i + $2) ))
    : $(((i += 1) - 1))
  done
  : $(( $1 = arr ))
  rest_loc_var $1 i arr
}
_array_sum() { # arr: $2, len: $3
  save_loc_var i sum
  : $(( sum = 0 ))
  : $(( i = 0 ))
  : $(( sum = 0 ))
  : $(( i = 0 ))
  while [ $i -lt $3 ] ; do
    : $((sum += _$(($2+i))))
    : $(((i += 1) - 1))
  done
  : $(( $1 = sum ))
  rest_loc_var $1 sum i
}
_main() {
  save_loc_var arr n3 n2 n1
  : $(( n1 = 0 ))
  : $(( n2 = 0 ))
  : $(( n3 = 0 ))
  : $(( arr = 0 ))
  _iota_array arr 0 50
  _string_len n1 $__0
  defstr __str_0 "Hello, world!"
  _string_sum n2 $__str_0
  _array_sum n3 $arr 50
  defstr __str_1 "n1 = %d\n"
  _printf __ $__str_1 $n1
  defstr __str_2 "n2 = %d\n"
  _printf __ $__str_2 $n2
  defstr __str_3 "n3 = %d\n"
  _printf __ $__str_3 $n3
  rest_loc_var $1 n1 n2 n3 arr
}

__result_loc=__ # Dummy result location
__=0 # Required for zsh

_main __ $__argc_for_main $__argv_for_main
