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
: $(( f2 = c2 = f1 = f = line = i = c = c1 = __g1 = 0 ))

# Character constants
__NEWLINE_CH=10
__LOWER_r=114

_emit_line() { # line: $2, f: $3
  save_loc_var __g1 c
  : $(( c = 0 ))
  defstr __str_0 "%d: "
  _printf __ $__str_0 $2
  while { _fgetc __g1 $3; [ $((c = __g1)) -ne 0 ]; } && [ $c -ne $_EOF ] && [ $c -ne $__NEWLINE_CH ] ; do
    _putchar __ $c
  done
  if [ $c -ne $_EOF ] ; then
    _putchar __ $__NEWLINE_CH
  fi
  : $(( $1 = c ))
  rest_loc_var $1 c __g1
}
_main() {
  save_loc_var i c2 c1 f2 f1
  : $(( f1 = 0 ))
  : $(( f2 = 0 ))
  : $(( c1 = 0 ))
  : $(( c2 = 0 ))
  : $(( i = 0 ))
  defstr __str_1 "six-cc-tests/fgetc.c"
  _fopen f1 $__str_1 $__LOWER_r
  defstr __str_2 "six-cc-tests/while-fun-call.c"
  _fopen f2 $__str_2 $__LOWER_r
  while [ 1 -ne 0 ] ; do
    _emit_line c1 $i $f1
    _emit_line c2 $i $f2
    if [ $c1 -eq $_EOF ] || [ $c2 -eq $_EOF ] ; then
      break
    fi
    : $((i += 1))
  done
  rest_loc_var $1 f1 f2 c1 c2 i
}

__result_loc=__ # Dummy result location
__=0 # Required for zsh

_main __ $__argc_for_main $__argv_for_main
