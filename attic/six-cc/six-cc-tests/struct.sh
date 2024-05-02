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
: $(( ll = max = node = sum = last = i = head = __g1 = 0 ))


# Defining struct fields of LinkedList
_val=0 _next=1 

_iota_linked_list() { # max: $2
  save_loc_var node last head i
  : $(( i = 0 ))
  : $(( head = 0 ))
  : $(( last = 0 ))
  : $(( node = 0 ))
  if [ $2 -eq 0 ] ; then
    : $(( $1 = _NULL ))
    rest_loc_var $1 i head last node
    return
  fi
  _malloc head 2
  : $(( _$((head+_val)) = 0 ))
  : $(( last = head ))
  : $(( i = 1 ))
  while [ $i -lt $2 ] ; do
    _malloc node 2
    : $(( _$((node+_val)) = i ))
    : $(( _$((node+_next)) = _NULL ))
    : $(( _$((last+_next)) = node ))
    : $(( last = node ))
    : $(((i += 1) - 1))
  done
  : $(( $1 = head ))
  rest_loc_var $1 i head last node
}
_linked_list_sum() { # head: $2
  save_loc_var sum head
  : $(( head = $2 ))
  : $(( sum = 0 ))
  : $(( sum = 0 ))
  while [ $head -ne $_NULL ] ; do
    : $((sum += _$((head+_val))))
    : $(( head = _$((head+_next)) ))
  done
  : $(( $1 = sum ))
  rest_loc_var $1 head sum
}
_linked_list_sum_except_last() { # head: $2
  save_loc_var sum head
  : $(( head = $2 ))
  : $(( sum = 0 ))
  : $(( sum = 0 ))
  while [ $head -ne $_NULL ] && [ $((_$((head+_next)))) -ne $_NULL ] ; do
    : $((sum += _$((head+_val))))
    : $(( head = _$((head+_next)) ))
  done
  : $(( $1 = sum ))
  rest_loc_var $1 head sum
}
_main() {
  save_loc_var __g1 ll
  : $(( ll = 0 ))
  _iota_linked_list ll 1000
  defstr __str_0 "Sum: %d\n"
  _linked_list_sum __g1 $ll
  _printf __ $__str_0 $__g1
  defstr __str_0 "Sum: %d\n"
  _linked_list_sum_except_last __g1 $_NULL
  _printf __ $__str_0 $__g1
  defstr __str_0 "Sum: %d\n"
  _linked_list_sum_except_last __g1 $ll
  _printf __ $__str_0 $__g1
  rest_loc_var $1 ll __g1
}

__result_loc=__ # Dummy result location
__=0 # Required for zsh

_main __ $__argc_for_main $__argv_for_main
