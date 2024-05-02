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
: $(( c = a = f = __g1 = 0 ))

# Character constants
__NEWLINE_CH=10
__UPPER_A=65
__UPPER_B=66
__UPPER_C=67
__UPPER_D=68
__UPPER_E=69
__UPPER_F=70
__UPPER_G=71
__UPPER_H=72
__UPPER_I=73
__UPPER_J=74
__UPPER_K=75
__UPPER_L=76
__UPPER_M=77
__UPPER_N=78
__UPPER_O=79
__UPPER_P=80
__UPPER_Q=81
__UPPER_R=82
__UPPER_S=83
__UPPER_T=84
__UPPER_U=85
__UPPER_V=86
__UPPER_W=87
__UPPER_X=88
__UPPER_Y=89
__UPPER_Z=90
__LOWER_a=97
__LOWER_b=98
__LOWER_c=99
__LOWER_d=100
__LOWER_e=101
__LOWER_f=102
__LOWER_g=103
__LOWER_h=104
__LOWER_i=105
__LOWER_j=106
__LOWER_k=107
__LOWER_l=108
__LOWER_m=109
__LOWER_n=110
__LOWER_o=111
__LOWER_p=112
__LOWER_q=113
__LOWER_r=114
__LOWER_s=115
__LOWER_t=116
__LOWER_u=117
__LOWER_v=118
__LOWER_w=119
__LOWER_x=120
__LOWER_y=121
__LOWER_z=122

# Defining enum Category
_LOWERCASE=1 _UPPERCASE=2 _NEWLINE=3 _OTHER=4 

_categorize_char() { # c: $2
  save_loc_var a
  : $(( a = 0 ))
  case $2 in
    $__LOWER_a|$__LOWER_b|$__LOWER_c|$__LOWER_d|$__LOWER_e|$__LOWER_f|$__LOWER_g|$__LOWER_h|$__LOWER_i|$__LOWER_j|$__LOWER_k|$__LOWER_l|$__LOWER_m|$__LOWER_n|$__LOWER_o|$__LOWER_p|$__LOWER_q|$__LOWER_r|$__LOWER_s|$__LOWER_t|$__LOWER_u|$__LOWER_v|$__LOWER_w|$__LOWER_x|$__LOWER_y|$__LOWER_z)
      : $(( a = _LOWERCASE ))
      : $(( $1 = _LOWERCASE ))
      rest_loc_var $1 a
      return
      ;;
    $__UPPER_A|$__UPPER_B|$__UPPER_C|$__UPPER_D|$__UPPER_E|$__UPPER_F|$__UPPER_G|$__UPPER_H|$__UPPER_I|$__UPPER_J|$__UPPER_K|$__UPPER_L|$__UPPER_M|$__UPPER_N|$__UPPER_O|$__UPPER_P|$__UPPER_Q|$__UPPER_R|$__UPPER_S|$__UPPER_T|$__UPPER_U|$__UPPER_V|$__UPPER_W|$__UPPER_X|$__UPPER_Y|$__UPPER_Z)
      : $(( $1 = _UPPERCASE ))
      rest_loc_var $1 a
      return
      ;;
    $__NEWLINE_CH)
      : $(( $1 = _NEWLINE ))
      rest_loc_var $1 a
      return
      ;;
    *)
      : $(( $1 = _OTHER ))
      rest_loc_var $1 a
      return
      ;;
  esac
  : $(( $1 = a ))
  rest_loc_var $1 a
}
_main() {
  save_loc_var __g1 c f
  : $(( f = 0 ))
  : $(( c = 0 ))
  defstr __str_0 "six-cc-tests/fgetc.c"
  _fopen f $__str_0 $__LOWER_r
  while _fgetc __g1 $f; [ $((c = __g1)) -ne $_EOF ] ; do
    defstr __str_1 "\'%c\' = %d: %d\n"
    _categorize_char __g1 $c
    _printf __ $__str_1 $c $c $__g1
  done
  rest_loc_var $1 f c __g1
}

__result_loc=__ # Dummy result location
__=0 # Required for zsh

_main __ $__argc_for_main $__argv_for_main
