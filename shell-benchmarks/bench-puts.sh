#!/bin/sh
set -e -u -f
LC_ALL=C

: $((res = str = 0))
_atoi() { let str $2
  let res
  res=0
  while [ $((_$str)) != $__NUL__ ] ; do
    res=$((((res * 10) + _$str) - __0__))
    : $((str += 1))
  done
  : $(($1 = res))
  endlet $1 res str
}

_c=0
: $((str = 0))
_puts_fast() { let str $2
  while [ $((_c = _$str)) != 0 ] ; do
    printf \\$((_c/64))$((_c/8%8))$((_c%8))
    : $((str += 1))
  done
  endlet $1 str
}

: $((str = 0))
_puts() { let str $2
  while [ $((_$str)) != 0 ] ; do
    printf \\$(((_$str)/64))$(((_$str)/8%8))$(((_$str)%8))
    : $((str += 1))
  done
  endlet $1 str
}

: $((str = count = argv_ = argc = 0))
_main() { let argc $2; let argv_ $3
  let count; let str
  count=1
  if [ $argc != 4 ] ; then
    printf "Usage: bench-puts <string> <count> <method>\n"
    exit 1
  fi
  str=$((_$((argv_ + 1))))
  _atoi count $((_$((argv_ + 2))))
  if [ $((_$((_$((argv_ + 3)) + 0)))) = $__f__ ] ; then
    while [ $(((count -= 1) + 1)) != 0 ] ; do
      _puts_fast __ $str
    done
  else
    while [ $(((count -= 1) + 1)) != 0 ] ; do
      _puts __ $str
    done
  fi
  endlet $1 str count argv_ argc
}

# Character constants
readonly __NUL__=0
readonly __0__=48
readonly __f__=102
# Runtime library
__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $(($1 = $__ALLOC + 1))
  : $((__ALLOC += $2 + 1))
}

__c2i_0=48
__c2i_1=49
__c2i_2=50
__c2i_3=51
__c2i_4=52
__c2i_5=53
__c2i_6=54
__c2i_7=55
__c2i_8=56
__c2i_9=57
__c2i_a=97
__c2i_b=98
__c2i_c=99
__c2i_d=100
__c2i_e=101
__c2i_f=102
__c2i_g=103
__c2i_h=104
__c2i_i=105
__c2i_j=106
__c2i_k=107
__c2i_l=108
__c2i_m=109
__c2i_n=110
__c2i_o=111
__c2i_p=112
__c2i_q=113
__c2i_r=114
__c2i_s=115
__c2i_t=116
__c2i_u=117
__c2i_v=118
__c2i_w=119
__c2i_x=120
__c2i_y=121
__c2i_z=122
__c2i_A=65
__c2i_B=66
__c2i_C=67
__c2i_D=68
__c2i_E=69
__c2i_F=70
__c2i_G=71
__c2i_H=72
__c2i_I=73
__c2i_J=74
__c2i_K=75
__c2i_L=76
__c2i_M=77
__c2i_N=78
__c2i_O=79
__c2i_P=80
__c2i_Q=81
__c2i_R=82
__c2i_S=83
__c2i_T=84
__c2i_U=85
__c2i_V=86
__c2i_W=87
__c2i_X=88
__c2i_Y=89
__c2i_Z=90
char_to_int() {
  case $1 in
    [[:alnum:]]) __c=$((__c2i_$1)) ;;
    ' ') __c=32 ;;
    '!') __c=33 ;;
    '"') __c=34 ;;
    '#') __c=35 ;;
    '$') __c=36 ;;
    '%') __c=37 ;;
    '&') __c=38 ;;
    "'") __c=39 ;;
    '(') __c=40 ;;
    ')') __c=41 ;;
    '*') __c=42 ;;
    '+') __c=43 ;;
    ',') __c=44 ;;
    '-') __c=45 ;;
    '.') __c=46 ;;
    '/') __c=47 ;;
    ':') __c=58 ;;
    ';') __c=59 ;;
    '<') __c=60 ;;
    '=') __c=61 ;;
    '>') __c=62 ;;
    '?') __c=63 ;;
    '@') __c=64 ;;
    '[') __c=91 ;;
    '\') __c=92 ;;
    ']') __c=93 ;;
    '^') __c=94 ;;
    '_') __c=95 ;;
    '`') __c=96 ;;
    '{') __c=123 ;;
    '|') __c=124 ;;
    '}') __c=125 ;;
    '~') __c=126 ;;
    *)
      __c=$(printf "%d" "'$1"); __c=$((__c > 0 ? __c : 256 + __c)) ;;
  esac
}

# Convert a Shell string to a C string
unpack_string() {
  __str="$2"
  _malloc $1 $((${#__str} + 1))
  __ptr=$(($1))
  __us_buf16=
  __us_buf256=
  while [ ! -z "$__str" ] || [ ! -z "$__us_buf256" ] ; do
  if [ -z "$__us_buf256" ]; then
    if [ ${#__str} -ge 256 ]; then
      __temp="${__str#????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????}"
      __us_buf256="${__str%"$__temp"}"
      __str="$__temp"
    else
      __us_buf256="$__str"
      __str=
    fi
  fi
  if [ -z "$__us_buf16" ]; then
    if [ ${#__us_buf256} -ge 16 ]; then
      __temp="${__us_buf256#????????????????}"
      __us_buf16="${__us_buf256%"$__temp"}"
      __us_buf256="$__temp"
    else
      __us_buf16="$__us_buf256"
      __us_buf256=
    fi
  fi
    while [ ! -z "$__us_buf16" ]; do
      case "$__us_buf16" in
        " "*) : $((_$__ptr = 32))  ;;
        "e"*) : $((_$__ptr = 101)) ;;
        "="*) : $((_$__ptr = 61))  ;;
        "t"*) : $((_$__ptr = 116)) ;;
        ";"*) : $((_$__ptr = 59))  ;;
        "i"*) : $((_$__ptr = 105)) ;;
        ")"*) : $((_$__ptr = 41))  ;;
        "("*) : $((_$__ptr = 40))  ;;
        "n"*) : $((_$__ptr = 110)) ;;
        "s"*) : $((_$__ptr = 115)) ;;
        "l"*) : $((_$__ptr = 108)) ;;
        "+"*) : $((_$__ptr = 43))  ;;
        "p"*) : $((_$__ptr = 112)) ;;
        "a"*) : $((_$__ptr = 97))  ;;
        "r"*) : $((_$__ptr = 114)) ;;
        "f"*) : $((_$__ptr = 102)) ;;
        "d"*) : $((_$__ptr = 100)) ;;
        "*"*) : $((_$__ptr = 42))  ;;
        *)
          char_to_int "${__us_buf16%"${__us_buf16#?}"}"
          : $((_$__ptr = __c))
          ;;
      esac
      __us_buf16=${__us_buf16#?}  # Remove the first character
      : $((__ptr += 1))           # Move to the next buffer position
    done
  done
  : $((_$__ptr = 0))
}

make_argv() {
  __argc=$1; shift;
  _malloc __argv $__argc # Allocate enough space for all elements. No need to initialize.
  __argv_ptr=$__argv

  while [ $# -ge 1 ]; do
    unpack_string _$__argv_ptr "$1"
    : $((__argv_ptr += 1))
    shift
  done
}

# Local variables
__=0
__SP=0
let() { # $1: variable name, $2: value (optional)
  : $((__SP += 1)) $((__$__SP=$1)) # Push
  : $(($1=${2-0}))                 # Init
}
endlet() { # $1: return variable
           # $2...: function local variables
  __ret=$1 # Don't overwrite return value
  : $((__tmp = $__ret))
  while [ $# -ge 2 ]; do
    : $(($2 = __$__SP)) $((__SP -= 1)); # Pop
    shift;
  done
  : $(($__ret=__tmp))   # Restore return value
}

# Setup argc, argv
__argc_for_main=$(($# + 1))
make_argv $__argc_for_main "$0" "$@"; __argv_for_main=$__argv
_main __ $__argc_for_main $__argv_for_main
