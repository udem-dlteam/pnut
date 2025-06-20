#!/bin/sh
set -e -u -f
LC_ALL=C

__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $(($1 = $__ALLOC + 1))
  : $((__ALLOC += $2 + 1))
}

defarr() { _malloc $1 $2; }

defarr _buf 1024

unpack_escaped_string() { # $1 = string, $2 = size (optional)
  __str="$1"
  # Allocates enough space for all characters, assuming that no character is escaped
  _malloc __addr $((${2:-${#__str} + 1}))
  __ptr=$__addr
  __end=$((__ptr + ${2:-${#__str} + 1})) # End of allocated memory
  while [ -n "$__str" ] ; do
    case "$__str" in
      '\'*)
        __str="${__str#?}" # Remove the current char from $__str
        case "$__str" in
          '0'*) __c=0 ;;
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
          *) echo "invalid escape in string: $__str"; exit 1 ;;
        esac
        __str="${__str#?}" # Remove the current char from $__str
        ;;
      *)
        __c=$(printf "%d" "'${__str%"${__str#?}"}"); __c=$((__c > 0 ? __c : 256 + __c))
        __str="${__str#?}" # Remove the current char from $__str
        ;;
    esac
    : $((_$__ptr = __c))
    : $((__ptr += 1))
  done
  while [ $__ptr -le $__end ]; do
    : $((_$__ptr = 0))
    : $((__ptr += 1))
  done
}

# Define a string, and return a reference to it in the varible taken as argument.
# If the variable is already defined, this function does nothing.
# Note that it's up to the caller to ensure that no 2 strings share the same variable.
defstr() { # $1 = variable name, $2 = string, $3 = size (optional)
  set +u # Necessary to allow the variable to be empty
  if [ $(($1)) -eq 0 ]; then
    unpack_escaped_string "$2" $3
    : $(($1 = __addr))
  fi
  set -u
}

defstr __str_0 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
_codes=$__str_0
: $((b3 = b2 = b1 = 0))
_encode() {
  let b1; let b2; let b3
  while [ 1 != 0 ]; do
    _getchar b1
    if [ $b1 -lt 0 ] ; then
      break
    fi
    _getchar b2
    printf \\$(((_$((_codes + (b1 >> 2))))/64))$(((_$((_codes + (b1 >> 2))))/8%8))$(((_$((_codes + (b1 >> 2))))%8))
    if [ $b2 -lt 0 ] ; then
      printf \\$(((_$((_codes + (0x3f & (b1 << 4)))))/64))$(((_$((_codes + (0x3f & (b1 << 4)))))/8%8))$(((_$((_codes + (0x3f & (b1 << 4)))))%8))
      printf "="
      printf "="
      break
    else
      printf \\$(((_$((_codes + (0x3f & ((b1 << 4) | (b2 >> 4))))))/64))$(((_$((_codes + (0x3f & ((b1 << 4) | (b2 >> 4))))))/8%8))$(((_$((_codes + (0x3f & ((b1 << 4) | (b2 >> 4))))))%8))
      _getchar b3
      if [ $b3 -lt 0 ] ; then
        printf \\$(((_$((_codes + (0x3f & (b2 << 2)))))/64))$(((_$((_codes + (0x3f & (b2 << 2)))))/8%8))$(((_$((_codes + (0x3f & (b2 << 2)))))%8))
        printf "="
        break
      else
        printf \\$(((_$((_codes + (0x3f & ((b2 << 2) | (b3 >> 6))))))/64))$(((_$((_codes + (0x3f & ((b2 << 2) | (b3 >> 6))))))/8%8))$(((_$((_codes + (0x3f & ((b2 << 2) | (b3 >> 6))))))%8))
        printf \\$(((_$((_codes + (0x3f & b3))))/64))$(((_$((_codes + (0x3f & b3))))/8%8))$(((_$((_codes + (0x3f & b3))))%8))
      fi
    fi
  done
  printf \\$(((__NEWLINE__)/64))$(((__NEWLINE__)/8%8))$(((__NEWLINE__)%8))
  endlet $1 b3 b2 b1
}

defarr _lut 256
: $((c = 0))
_get() {
  let c
  while _getchar c; [ $c -ge 0 ]; do
    if [ $((c = _$((_lut + c)))) -ge 0 ] ; then
      break
    fi
  done
  : $(($1 = c))
  endlet $1 c
}

: $((c4 = c3 = c2 = c1 = i = 0))
_decode() {
  let i; let c1; let c2; let c3; let c4
  i=0
  while [ $i -lt 256 ]; do
    : $((_$((_lut + i)) = -1))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 64 ]; do
    : $((_$((_lut + 0xff & _$((_codes + i)))) = i))
    : $((i += 1))
  done
  while _get c1; [ $c1 -ge 0 ]; do
    if _get c2; [ $c2 -lt 0 ] ; then
      exit 1
    fi
    printf \\$((((c1 << 2) | (c2 >> 4))/64))$((((c1 << 2) | (c2 >> 4))/8%8))$((((c1 << 2) | (c2 >> 4))%8))
    if _get c3; [ $c3 -lt 0 ] ; then
      break
    fi
    printf \\$(((0xff & ((c2 << 4) | (c3 >> 2)))/64))$(((0xff & ((c2 << 4) | (c3 >> 2)))/8%8))$(((0xff & ((c2 << 4) | (c3 >> 2)))%8))
    if _get c4; [ $c4 -lt 0 ] ; then
      break
    fi
    printf \\$(((0xff & ((c3 << 6) | c4))/64))$(((0xff & ((c3 << 6) | c4))/8%8))$(((0xff & ((c3 << 6) | c4))%8))
  done
  endlet $1 c4 c3 c2 c1 i
}

: $((myargv = argc = 0))
_main() { let argc $2; let myargv $3
  if [ $argc = 1 ] ; then
    _encode __
  elif [ $argc = 2 ] && [ $((_$((_$((myargv + 1)) + 0)))) = $__MINUS__ ] && [ $((_$((_$((myargv + 1)) + 1)))) = $__d__ ] && [ $((_$((_$((myargv + 1)) + 2)))) = $__NUL__ ] ; then
    _decode __
  else
    exit 1
  fi
  : $(($1 = 0))
  endlet $1 myargv argc
}

# Character constants
readonly __NUL__=0
readonly __NEWLINE__=10
readonly __MINUS__=45
readonly __d__=100
# Runtime library
__stdin_buf=
__stdin_line_ending=0 # Line ending, either -1 (EOF) or 10 ('\n')
_getchar() {
  if [ -z "$__stdin_buf" ]; then          # need to get next line when buffer empty
    if [ $__stdin_line_ending != 0 ]; then  # Line is empty, return line ending
      : $(($1 = __stdin_line_ending))
      __stdin_line_ending=0                  # Reset line ending for next getchar call
      return
    fi
    IFS=                                            # don't split input
    if read -r __stdin_buf ; then                   # read next line into $__stdin_buf
      if [ -z "$__stdin_buf" ] ; then               # an empty line implies a newline character
        : $(($1 = 10))                              # next getchar call will read next line
        return
      fi
      __stdin_line_ending=10
    else
      if [ -z "$__stdin_buf" ] ; then               # EOF reached when read fails
        : $(($1 = -1))
        return
      else
        __stdin_line_ending=-1
      fi
    fi
  fi
  __c=$(printf "%d" "'${__stdin_buf%"${__stdin_buf#?}"}"); __c=$((__c > 0 ? __c : 256 + __c))
  : $(($1 = __c))
    __stdin_buf="${__stdin_buf#?}"                  # remove the current char from $__stdin_buf
}

# Unpack a Shell string into an appropriately sized buffer
unpack_string() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?
  __fgetc_buf=$1
  __buffer=$2
  __ends_with_eof=$3
  while [ ! -z "$__fgetc_buf" ]; do
    __c=$(printf "%d" "'${__fgetc_buf%"${__fgetc_buf#?}"}"); __c=$((__c > 0 ? __c : 256 + __c))
    : $((_$__buffer = __c))
    __fgetc_buf=${__fgetc_buf#?}      # Remove the first character
    : $((__buffer += 1))              # Move to the next buffer position
  done

  if [ $__ends_with_eof -eq 0 ]; then # Ends with newline and not EOF?
    : $((_$__buffer = 10))            # Line ends with newline
    : $((__buffer += 1))
  fi
  : $((_$__buffer = 0))               # Then \0
}

make_argv() {
  __argc=$1; shift;
  _malloc __argv $__argc # Allocate enough space for all elements. No need to initialize.
  __argv_ptr=$__argv

  while [ $# -ge 1 ]; do
    _malloc _$__argv_ptr $((${#1} + 1))
    unpack_string "$1" $((_$__argv_ptr)) 1
    : $((__argv_ptr += 1))
    shift
  done
}

# Local variables
__=0
__SP=0
let() { # $1: variable name, $2: value (optional)
  : $((__$((__SP += 1))=$1)) # Push
  : $(($1=${2-0}))           # Init
}
endlet() { # $1: return variable
           # $2...: function local variables
  __ret=$1 # Don't overwrite return value
  : $((__tmp = $__ret))
  while [ $# -ge 2 ]; do
    : $(($2 = __$(((__SP -= 1) + 1)))) # Pop
    shift;
  done
  : $(($__ret=__tmp))   # Restore return value
}

__code=0; # Exit code
make_argv $(($# + 1)) "$0" "$@" # Setup argc/argv
_main __code $(($# + 1)) $__argv
exit $__code
