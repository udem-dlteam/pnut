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
: $(( len = h = s = hash = ix = MAX_SIZE = f = __g1 = 0 ))


_hash() { # s: $2
  save_loc_var ix hash
  : $(( hash = 0 ))
  : $(( ix = 0 ))
  : $(( hash = 0 ))
  : $(( ix = 0 ))
  while [ $((_$(($2+ix)))) -ne 0 ] ; do
    : $(( hash = (((_$(($2+ix)) + (hash << 6)) + (hash << 16)) - hash) ))
    : $(( hash = (hash & 65535) ))
    : $(((ix += 1) - 1))
  done
  : $(( $1 = ix ))
  rest_loc_var $1 hash ix
}
_main() {
  save_loc_var __g1 h len s f MAX_SIZE
  : $(( MAX_SIZE = 0 ))
  : $(( f = 0 ))
  : $(( s = 0 ))
  : $(( len = 0 ))
  : $(( h = 0 ))
  : $(( MAX_SIZE = 200 ))
  _malloc s $MAX_SIZE
  defstr __str_0 "six-cc-tests/close.c"
  _open f $__str_0 0
  _read len $f $s $((MAX_SIZE - 1))
  : $(( _$((s+len)) = 0 ))
  _close __ $f
  defstr __str_1 "Read content: %s\n"
  _printf __ $__str_1 $s
  defstr __str_2 "File descriptor: %s\n"
  _printf __ $__str_2 $f
  defstr __str_3 "Quote: \"\n"
  _printf __ $__str_3 $len
  defstr __str_4 "Backslash: \\\\\n"
  _printf __ $__str_4 $len
  defstr __str_5 "Read len: %d\n"
  _printf __ $__str_5 $len
  defstr __str_6 "Read result: %.*s\n"
  _printf __ $__str_6 $len $s
  defstr __str_7 "hash: %d\n"
  _hash __g1 $s
  _printf __ $__str_7 $__g1
  rest_loc_var $1 MAX_SIZE f s len h __g1
}

__result_loc=__ # Dummy result location
__=0 # Required for zsh

_main __ $__argc_for_main $__argv_for_main
