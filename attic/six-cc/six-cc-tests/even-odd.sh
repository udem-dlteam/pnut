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
: $(( n1 = n2 = a = number = __g1 = 0 ))


_abs() { # number: $2
  if [ $2 -lt 0 ] ; then
    : $(( $1 = -($2) ))
    return
  fi
  : $(( $1 = $2 ))
}
_even() { # number: $2
  save_loc_var __g1 a
  : $(( a = 0 ))
  if [ $2 -eq 0 ] ; then
    : $(( $1 = 1 ))
    rest_loc_var $1 a
    return
  fi
  _abs __g1 $2
  _odd $1 $((__g1 - 1))
  rest_loc_var $1 a __g1
}
_odd() { # number: $2
  save_loc_var __g1 a
  : $(( a = 0 ))
  if [ $2 -eq 0 ] ; then
    : $(( $1 = 0 ))
    rest_loc_var $1 a
    return
  fi
  _abs __g1 $2
  _even $1 $((__g1 - 1))
  rest_loc_var $1 a __g1
}
_main() {
  save_loc_var n2 n1
  : $(( n1 = 0 ))
  : $(( n2 = 0 ))
  _even n1 10
  _odd n2 10
  defstr __str_0 "n1 = %d\n"
  _printf __ $__str_0 $n1
  defstr __str_1 "n2 = %d\n"
  _printf __ $__str_1 $n2
  rest_loc_var $1 n1 n2
}

__result_loc=__ # Dummy result location
__=0 # Required for zsh

_main __ $__argc_for_main $__argv_for_main
