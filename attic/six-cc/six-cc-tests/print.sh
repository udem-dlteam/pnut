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
: $(( str = 0 ))

# Character constants
__UPPER_A=65
__UPPER_L=76
__UPPER_O=79

_main() {
  save_loc_var str
  : $(( str = 0 ))
  defstr __str_0 "ABCDEFHIJKLMNOPQRSTUVWXYZ"
  : $(( str = __str_0 ))
  defstr __str_1 "%d\n"
  _printf __ $__str_1 42
  defstr __str_2 "Beau %s\n"
  defstr __str_3 "velo"
  _printf __ $__str_2 $__str_3
  defstr __str_4 "Beau %s %s\n"
  defstr __str_5 "velo (%s)"
  defstr __str_6 "bleu"
  _printf __ $__str_4 $__str_5 $__str_6
  defstr __str_7 "Beau %s %s %s\n"
  defstr __str_5 "velo (%s)"
  defstr __str_6 "bleu"
  defstr __str_8 "rouge"
  _printf __ $__str_7 $__str_5 $__str_6 $__str_8
  defstr __str_9 "%c"
  _printf __ $__str_9 $__UPPER_A
  defstr __str_9 "%c"
  _printf __ $__str_9 $__UPPER_L
  defstr __str_9 "%c"
  _printf __ $__str_9 $__UPPER_L
  defstr __str_10 "%c\n"
  _printf __ $__str_10 $__UPPER_O
  defstr __str_11 "Allo in hex: 0x%x 0x%x 0x%x 0x%x\n"
  _printf __ $__str_11 $__UPPER_A $__UPPER_L $__UPPER_L $__UPPER_O
  defstr __str_12 "_\n"
  _printf __ $__str_12
  defstr __str_13 "alphabet:\n%s\n"
  _printf __ $__str_13 $str
  defstr __str_14 "4 first letters of the alphabet with .*s:\n%.*s\n"
  _printf __ $__str_14 4 $str
  defstr __str_15 "4 first letters of the alphabet with 0.4s:\n%0.4s\n"
  _printf __ $__str_15 $str
  defstr __str_16 "Last 4 letters of the alphabet with padding:\n%26.s\n"
  defstr __str_0 "ABCDEFHIJKLMNOPQRSTUVWXYZ"
  _printf __ $__str_16 $((__str_0 + 20))
  defstr __str_17 "The alphabet twice: %s %s\n"
  defstr __str_0 "ABCDEFHIJKLMNOPQRSTUVWXYZ"
  defstr __str_0 "ABCDEFHIJKLMNOPQRSTUVWXYZ"
  _printf __ $__str_17 $__str_0 $__str_0
  rest_loc_var $1 str
}

__result_loc=__ # Dummy result location
__=0 # Required for zsh

_main __ $__argc_for_main $__argv_for_main
