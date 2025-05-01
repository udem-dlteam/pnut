#!/bin/sh
set -e -u -f
LC_ALL=C

: $((__t1 = n = 0))
_main() {
  let n; let __t1
  n=0
  while :; do
    printf "Enter a non-zero single-digit number:\r\n"
    _getchar n
    while _getchar __t1; [ $__NEWLINE__ != $__t1 ]; do
      :
    done
    [ $n = $__0__ ] || [ $((!((n >= __0__) && (n <= __9__)))) != 0 ] || break
  done
  printf "You entered "
  printf \\$((n/64))$((n/8%8))$((n%8))
  printf ": bye bye!\r\n"
  endlet $1 __t1 n
}

# Character constants
readonly __NEWLINE__=10
readonly __0__=48
readonly __9__=57
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
_main __code
exit $__code
