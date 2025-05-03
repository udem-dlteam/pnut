#!/bin/sh
set -e -u -f
LC_ALL=C

: $((c = 0))
_is_word_separator() { let c $2
  : $(($1 = (c == __SPACE__) || (c == __NEWLINE__) || (c == __TAB__)))
  endlet $1 c
}

: $((last_sep = sep = c = chars = words = lines = 0))
_main() {
  let lines; let words; let chars; let c; let sep; let last_sep
  lines=0
  words=0
  chars=0
  sep=0
  last_sep=0
  while _getchar c; [ $c != -1 ]; do
    : $((chars += 1))
    if [ $c = $__NEWLINE__ ] ; then
      : $((lines += 1))
    fi
    _is_word_separator sep $c
    if [ $sep != 0 ] && [ $((! last_sep)) != 0 ] ; then
      : $((words += 1))
    fi
    last_sep=$sep
  done
  printf "%d %d %d\n" $lines $words $chars
  endlet $1 last_sep sep c chars words lines
}

# Character constants
readonly __TAB__=9
readonly __NEWLINE__=10
readonly __SPACE__=32
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
