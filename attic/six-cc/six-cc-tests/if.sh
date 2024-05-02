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
: $(( x = y = 0 ))


_main() {
  save_loc_var y x
  : $(( x = 0 ))
  : $(( y = 0 ))
  : $(( x = 42 ))
  : $(( y = 128 ))
  if [ $x -lt $y ] ; then
    defstr __str_0 "if (x < y)\n"
    _printf __ $__str_0
  else
    defstr __str_1 "else (x >= y)\n"
    _printf __ $__str_1
  fi
  if [ $x -gt $y ] ; then
    defstr __str_2 "x > y\n"
    _printf __ $__str_2
  elif [ $x -eq $y ] ; then
    defstr __str_3 "else if (x == y)\n"
    _printf __ $__str_3
  fi
  if [ $x -gt $y ] ; then
    defstr __str_4 "if (x > y)\n"
    _printf __ $__str_4
  elif [ $x -eq $y ] ; then
    defstr __str_3 "else if (x == y)\n"
    _printf __ $__str_3
  else
    defstr __str_5 "else (x < y)\n"
    _printf __ $__str_5
  fi
  rest_loc_var $1 x y
}

__result_loc=__ # Dummy result location
__=0 # Required for zsh

_main __ $__argc_for_main $__argv_for_main
