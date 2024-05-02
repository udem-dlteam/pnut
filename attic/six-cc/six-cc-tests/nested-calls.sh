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
: $(( i = x = y = __g1 = __g2 = __g3 = 0 ))


_abc() { # x: $2
  : $(( $1 = ($2 + 1) ))
}
_def() { # x: $2
  : $(( $1 = ($2 * 2) ))
}
_hij() { # x: $2, y: $3
  : $(( $1 = ($2 * $3) ))
}
defglo _counter 0
: $(( _counter = 10 ))
_decrement() { # i: $2
  save_loc_var i
  : $(( i = $2 ))
  : $(((i -= 1) + 1))
  : $(( $1 = i ))
  rest_loc_var $1 i
}
defarr _r 2801
_main() {
  save_loc_var __g3 __g2 __g1
  _abc __ 123
  _abc __g1 123
  : $(( _$((_r+__g1)) = 2000 ))
  defstr __str_0 "%d\n"
  _abc __g1 123
  _printf __ $__str_0 $((_$((_r+__g1))))
  while [ $(((_counter -= 1) + 1)) -ne 0 ] ; do
    defstr __str_1 "Counter: %d\n"
    _printf __ $__str_1 $_counter
  done
  defstr __str_0 "%d\n"
  _abc __g1 42
  _printf __ $__str_0 $__g1
  defstr __str_0 "%d\n"
  _def __g2 123
  _abc __g1 $__g2
  _printf __ $__str_0 $__g1
  defstr __str_0 "%d\n"
  _abc __g2 123
  _def __g3 123
  _hij __g1 $__g2 $__g3
  _printf __ $__str_0 $__g1
  defstr __str_0 "%d\n"
  _abc __g2 123
  _def __g3 123
  _hij __g1 $__g2 $__g3
  _abc __g2 123
  _printf __ $__str_0 $((__g1 + __g2))
  defstr __str_2 "%d %d\n"
  _hij __g1 2 3
  _abc __g2 123
  _printf __ $__str_2 $__g1 $__g2
  rest_loc_var $1 __g1 __g2 __g3
}

__result_loc=__ # Dummy result location
__=0 # Required for zsh

_main __ $__argc_for_main $__argv_for_main
