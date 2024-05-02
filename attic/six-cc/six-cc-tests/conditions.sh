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
: $(( k = j = i = p = 0 ))

# Character constants
__DIGIT_0=48
__DIGIT_9=57
__UPPER_A=65
__UPPER_Z=90
__UNDERSCORE_CH=95
__LOWER_a=97
__LOWER_z=122

_main() {
  save_loc_var p k j i
  : $(( i = 0 ))
  : $(( j = 5 ))
  : $(( k = 12 ))
  defstr __str_1 "_hello"
  : $(( p = __str_1 ))
  if { [ $((_$p)) -ge $__LOWER_a ] && [ $((_$p)) -le $__LOWER_z ]; } || { [ $((_$p)) -ge $__UPPER_A ] && [ $((_$p)) -le $__UPPER_Z ]; } || { [ $((_$p)) -ge $__DIGIT_0 ] && [ $((_$p)) -le $__DIGIT_9 ]; } || [ $((_$p)) -eq $__UNDERSCORE_CH ] ; then
    defstr __str_0 "p: %c\n"
    _printf __ $__str_0 $((_$p))
  fi
  if { [ $((_$p)) -ge $__LOWER_a ] || [ $((_$p)) -le $__LOWER_z ]; } && { [ $((_$p)) -ge $__UPPER_A ] || [ $((_$p)) -le $__UPPER_Z ]; } && [ $((!(_$p >= __DIGIT_0) || (_$p <= __DIGIT_9))) -ne 0 ] && [ $((_$p)) -eq $__UNDERSCORE_CH ] ; then
    defstr __str_0 "p: %c\n"
    _printf __ $__str_0 $((_$p))
  fi
  rest_loc_var $1 i j k p
}

__result_loc=__ # Dummy result location
__=0 # Required for zsh

_main __ $__argc_for_main $__argv_for_main
