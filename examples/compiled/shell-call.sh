#!/bin/sh
set -e -u

################################################################################
#
#                      Bindings for POSIX shell utilities
#
# ############################# Calling convention #############################
#
# The calling convention is the following:
#
# 1. C functions are prefixed with an underscore when compiled to shell.
#
# 2. The first argument is the return variable, it indicates where the return
#    value should be stored. The other arguments are the function arguments.
#
# 3. Variables are globally scoped, and the callee is responsible for restoring
#    the initial values of local variables before returning. Variables starting
#    with `_` are global variables, variables starting with `__` are reserved
#    for the runtime library, and `___` is free to be used by the user.
#
# 4. The arguments and return value can either be immediates (int) or pointers.
#    Shell strings must always be converted to the C string format before being
#    returned using the `unpack_string` function.
#
# ############################# String conversions #############################
#
# To help with the calling convention, the `pack_string` and `unpack_string`
# functions can be used to convert to/from shell strings and C strings. For
# functions that return multiple strings, the `unpack_lines` function can be
# used to unpack them into an array of strings.
#
# ############################# Memory allocation ##############################
#
# Memory can be dynamically allocated using the `_malloc` function, and freed
# using the `_free` function. For long running scripts, it is recommended to
# free memory as it tends to slow down the execution of scripts. Because POSIX
# shell requires 32-bit signed integers, the maximum amount of memory that can
# be allocated is 2^31 - 1 words. In practice, most shells will become unusably
# slow or run out of memory before reaching this limit.
#
# Because memory is allocated using a simple bump allocator, address space is
# never reclaimed. Programs that allocate over 2^31 - 1 words may need to manage
# blocks of memory manually to avoid running out of memory.
#
# Because the memory is word-addressable, pointers, integers and characters all
# occupy the same amount of memory. Keep that in mind when allocating
# structures.
#
################################################################################

# Like `unpack_string` but for multiple strings separated by newlines.
# Returns a pointer to the first string in the array, and null terminates the
# array.
unpack_lines() {
  ___i=1 # Account for null delimiter
  IFS=
  for ___line in $2; do
    : $((___i += 1))
  done
  _malloc $1 $___i
  ___i=0
  for ___line in $2; do
    unpack_string _$(($1 + ___i))  "$___line"
    : $((___i += 1))
  done
  : $((_$(($1 + ___i)) = 0)) # Null delimiter
}

_cat() { # $2 = file (char*)
  pack_string $2
  cat $__res
}

# Return the current date with the format "YYYY-MM-DDTHH:MM:SS"
_date() {
  ___date=$(date -Iseconds)
  unpack_string $1 "$___date"
}

_pwd() {
  ___pwd=$(pwd)
  unpack_string $1 "$___pwd"
}

# Example of a variadic function.
# This function can take an optional string argument for the directory to list.
_ls() { # $2 = dir (char*)
  __res=
  if [ $# -eq 2 ]; then
    pack_string $2
  fi
  ___lines=$(ls $__res)
  unpack_lines $1 "$___lines"
}

_touch() { # $2 = file (char*)
  pack_string $2
  touch $__res
}

# Create a directory and return the exit code.
_mkdir() { # $2 = dir (char*)
  pack_string $2
  set +e # Ignore errors
  mkdir -p $__res
  : $(($1 = $?)) # Return the exit code
  set -e # Restore set -e
}

_file_permission() { # $2 = file (char*)
  pack_string $2
  ___perms=$(ls -l $__res | cut -c1-10) # Produce -rwxr-xr-x
  unpack_string $1 "$___perms"
}

_chmod() { # $2 = mode (int), $3 = file (char*)
  pack_string $3
  set +e # Ignore errors
  chmod "$2" $__res
  : $(($1 = $?)) # Return the exit code
  set -e # Restore set -e
}

_wc() { # $2 = file (char*), $3 = lines addr (int*), $4 = words addr (int*), $5 = chars addr (int*)
  pack_string $2
  __res=$(wc $__res | read -r ___lines ___words ___chars _)
  # Write result to the addresses
  : $((_$3 = ___lines))
  : $((_$4 = ___words))
  : $((_$5 = ___chars))
}

: $((i = arr = 0))
_print_array() { let arr $2
  let i
  i=0
  while [ $((_$((arr + i)))) != 0 ] ; do
    _put_pstr __ $((_$((arr + i))))
    printf "\n" 
    : $(((i += 1) - 1))
  done
  endlet $1 i arr
}

: $((__t1 = 0))
_main() {
  let __t1
  _ls __t1 
  _print_array __ $__t1
  printf "pwd: " 
  _pwd __t1 
  _put_pstr __ $__t1
  printf "\n" 
  defstr __str_0 "examples/shell-call.c"
  _cat __ $__str_0
  defstr __str_1 "examples/fib.c"
  _cat __ $__str_1
  endlet $1 __t1
}

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
    [a-zA-Z0-9]) __c=$((__c2i_$1)) ;;
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
      __c=$(LC_CTYPE=C printf "%d" "'$1")
  esac
}

unpack_escaped_string() {
  __buf="$1"
  # Allocates enough space for all characters, assuming that no character is escaped
  _malloc __addr $((${#__buf} + 1))
  __ptr=$__addr
  while [ -n "$__buf" ] ; do
    case "$__buf" in
      '\'*)
        __buf="${__buf#?}"               # remove the current char from $__buf
        case "$__buf" in
          'a'*) __c=7 ;;
          'b'*) __c=8 ;;
          'f'*) __c=12 ;;
          'n'*) __c=10 ;;
          'r'*) __c=13 ;;
          't'*) __c=9 ;;
          'v'*) __c=11 ;;
          '\'*) __c=92 ;;
          '"'*) __c=34 ;;
          "'"*) __c=39 ;;
          '?'*) __c=63 ;;
          '$'*) __c=36 ;; # Not in C, used to escape variable expansion between double quotes
          *) echo "invalid escape in string: $__char"; exit 1 ;;
        esac
        __buf="${__buf#?}"               # remove the current char from $__buf
        ;;
      *)
        char_to_int "${__buf%"${__buf#?}"}"
        __buf="${__buf#?}"                  # remove the current char from $__buf
        ;;
    esac
    : $((_$__ptr = __c))
    : $((__ptr += 1))
  done
  : $((_$__ptr = 0))
}

# Define a string, and return a reference to it in the varible taken as argument.
# If the variable is already defined, this function does nothing.
# Note that it's up to the caller to ensure that no 2 strings share the same variable.
defstr() { # $1 = variable name, $2 = string
  set +u # Necessary to allow the variable to be empty
  if [ $(($1)) -eq 0 ]; then
    unpack_escaped_string "$2"
    : $(($1 = __addr))
  fi
  set -u
}

_put_pstr() {
  : $(($1 = 0)); shift # Return 0
  __addr=$1; shift
  while [ $((_$__addr)) != 0 ]; do
    printf \\$((_$__addr/64))$((_$__addr/8%8))$((_$__addr%8))
    : $((__addr += 1))
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

int_to_char() {
  case $1 in
    48|49|50|51|52|53|54|55|56|57) __char=$(($1 - 48)) ;;
    97)  __char="a" ;;
    98)  __char="b" ;;
    99)  __char="c" ;;
    100) __char="d" ;;
    101) __char="e" ;;
    102) __char="f" ;;
    103) __char="g" ;;
    104) __char="h" ;;
    105) __char="i" ;;
    106) __char="j" ;;
    107) __char="k" ;;
    108) __char="l" ;;
    109) __char="m" ;;
    110) __char="n" ;;
    111) __char="o" ;;
    112) __char="p" ;;
    113) __char="q" ;;
    114) __char="r" ;;
    115) __char="s" ;;
    116) __char="t" ;;
    117) __char="u" ;;
    118) __char="v" ;;
    119) __char="w" ;;
    120) __char="x" ;;
    121) __char="y" ;;
    122) __char="z" ;;
    65)  __char="A" ;;
    66)  __char="B" ;;
    67)  __char="C" ;;
    68)  __char="D" ;;
    69)  __char="E" ;;
    70)  __char="F" ;;
    71)  __char="G" ;;
    72)  __char="H" ;;
    73)  __char="I" ;;
    74)  __char="J" ;;
    75)  __char="K" ;;
    76)  __char="L" ;;
    77)  __char="M" ;;
    78)  __char="N" ;;
    79)  __char="O" ;;
    80)  __char="P" ;;
    81)  __char="Q" ;;
    82)  __char="R" ;;
    83)  __char="S" ;;
    84)  __char="T" ;;
    85)  __char="U" ;;
    86)  __char="V" ;;
    87)  __char="W" ;;
    88)  __char="X" ;;
    89)  __char="Y" ;;
    90)  __char="Z" ;;
    32)  __char=" " ;;
    33)  __char="!" ;;
    34)  __char="\"" ;;
    35)  __char="#" ;;
    36)  __char="$" ;;
    37)  __char="%" ;;
    38)  __char="&" ;;
    39)  __char="'" ;;
    40)  __char="(" ;;
    41)  __char=")" ;;
    42)  __char="*" ;;
    43)  __char="+" ;;
    44)  __char="," ;;
    45)  __char="-" ;;
    46)  __char="." ;;
    47)  __char="/" ;;
    58)  __char=":" ;;
    59)  __char=";" ;;
    60)  __char="<" ;;
    61)  __char="=" ;;
    62)  __char=">" ;;
    63)  __char="?" ;;
    64)  __char="@" ;;
    91)  __char="[" ;;
    92)  __char="\\" ;;
    93)  __char="]" ;;
    94)  __char="^" ;;
    95)  __char="_" ;;
    96)  __char="\`" ;;
    123) __char="{" ;;
    124) __char="|" ;;
    125) __char="}" ;;
    126) __char="~" ;;
    10)  __char="\n" ;;
    *)
      echo "Invalid character code: $1" ; exit 1
      __char=$(printf "\\$(printf "%o" "$1")") ;;
  esac
}

# Convert a pointer to a C string to a Shell string.
# $__res is set to the result, and $__len is set to the length of the string.
pack_string() { # $1 = string address, $2 = end of string delimiter (default to null), $3 = max length (default to 100000000) 
  __addr=$1; 
  __max_len=100000000
  __delim=0
  __len=0
  __res=""
  if [ $# -ge 2 ] ; then __delim=$2   ; fi # Optional end of string delimiter
  if [ $# -ge 3 ] ; then __max_len=$3 ; fi # Optional max length
  while [ $((_$__addr)) != $__delim ] && [ $__max_len -gt $__len ] ; do
    __char=$((_$__addr))
    __addr=$((__addr + 1))
    __len=$((__len + 1))
    case $__char in
      10) __res="$__res\n" ;; # 10 == '\n'
      *)        int_to_char "$__char"
        __res="$__res$__char" ;;
    esac
  done
}

# Convert a Shell string to a C string
unpack_string() {
  __str="$2"
  _malloc $1 $((${#__str} + 1))
  __ptr=$(($1))
  while [ -n "$__str" ] ; do
    # Remove first char from string
    __tail="${__str#?}"
    # Remove all but first char
    __char="${__str%"$__tail"}"
    # Convert char to ASCII
    char_to_int "$__char"
    # Write character to memory
    : $((_$__ptr = __c))
    # Continue with rest of string
    : $((__ptr += 1))
    __str="$__tail"
  done
  : $((_$__ptr = 0))
}

_main __
