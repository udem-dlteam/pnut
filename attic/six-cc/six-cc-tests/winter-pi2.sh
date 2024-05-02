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
: $(( newline2 = x = z = newline = y = 0 ))


defarr _r 2801
defglo _i 0
defglo _k 0
defglo _b 0
defglo _d 0
defglo _c 0
_main() {
  save_loc_var newline2 newline
  : $(( newline = 0 ))
  : $(( newline2 = 0 ))
  _identity newline 10 2 3
  while [ $_i -lt 2800 ] ; do
    : $(( _$((_r+_i)) = 2000 ))
    : $(((_i += 1) - 1))
  done
  : $(( _$((_r+_i)) = 0 ))
  : $(( _k = 280 ))
  while [ $_k -gt 0 ] ; do
    : $(( _d = 0 ))
    : $(( _i = _k ))
    while : ; do
      : $(( _d = (_d + (_$((_r+_i)) * 10000)) ))
      : $(( _b = ((2 * _i) - 1) ))
      : $(( _$((_r+_i)) = (_d % _b) ))
      : $(( _d = (_d / _b) ))
      : $(((_i -= 1) + 1))
      if [ $_i -eq 0 ] ; then
        break
      fi
      : $(( _d = (_d * _i) ))
    done
    _putchar __ $((48 + (((_c + (_d / 10000)) / 1000) % 10)))
    _putchar __ $((48 + (((_c + (_d / 10000)) / 100) % 10)))
    _putchar __ $((48 + (((_c + (_d / 10000)) / 10) % 10)))
    _putchar __ $((48 + ((_c + (_d / 10000)) % 10)))
    : $(( _c = (_d % 10000) ))
    : $(( _k = (_k - 14) ))
  done
  _putchar __ $newline
  : $(( $1 = 0 ))
  rest_loc_var $1 newline newline2
}
_identity() { # x: $2, y: $3, z: $4
  : $(( $1 = $2 ))
}

__result_loc=__ # Dummy result location
__=0 # Required for zsh

_main __ $__argc_for_main $__argv_for_main
