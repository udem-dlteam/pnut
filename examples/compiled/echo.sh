#!/bin/sh
set -e -u

: $((__t1 = c = 0))
_main() {
  let c; let __t1
  while _getchar __t1 ; [ $((c = __t1)) != -1 ] ; do
    printf \\$((c/64))$((c/8%8))$((c%8))
  done
  endlet $1 __t1 c
}

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
  __c=$(LC_CTYPE=C printf "%d" "'${__stdin_buf%"${__stdin_buf#?}"}")
  : $(($1 = __c))
    __stdin_buf="${__stdin_buf#?}"                  # remove the current char from $__stdin_buf
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

_main __
