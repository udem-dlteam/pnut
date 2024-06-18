set -e -u

#################################### C code ####################################
# // wc utility in C. Read from stdin and count lines, words, and characters.
# // #define is_word_separator(c) ((c) == ' ' || (c) == '\n' || (c) == '\t')
# int is_word_separator(char c) {
#   return c == ' ' || c == '\n' || c == '\t';
# }
################################# End of C code ################################
: $((c = 0))
_is_word_separator() { # c: $2
  let c
  c=$2
  : $(( $1 = (c == __CH_SPACE) || (c == __CH_NEWLINE) || (c == __CH_TAB) ))
  endlet $1 c
}

#################################### C code ####################################
# void main() {
#   int lines = 0, words = 0, chars = 0;
#   char c;
#   int sep = 0, last_sep = 0;
#   while ((c = getchar()) != -1) {
#     chars += 1;
#     if (c == '\n') lines += 1;
#     sep = is_word_separator(c);
#     if (sep && !last_sep) {
#       words += 1;
#     }
#     last_sep = sep;
#   }
#   printf("%d %d %d\n", lines, words, chars);
# }
################################# End of C code ################################
: $((__g1 = last_sep = sep = c = chars = words = lines = 0))
_main() {
  let lines; let words; let chars; let c; let sep; let last_sep; let __g1
  lines=0
  words=0
  chars=0
  c=0
  sep=0
  last_sep=0
  while _getchar __g1 ; [ $((c = __g1)) != -1 ] ; do
    : $((chars += 1))
    if [ $c = $__CH_NEWLINE ] ; then
      : $((lines += 1))
    fi
    _is_word_separator sep $c
    if [ $sep -ne 0 ] && [ $(( !last_sep )) -ne 0 ] ; then
      : $((words += 1))
    fi
    last_sep=$sep
  done
  printf "%d %d %d\n" $lines $words $chars
  endlet $1 __g1 last_sep sep c chars words lines
}

# Character constants
readonly __CH_TAB=9
readonly __CH_NEWLINE=10
readonly __CH_SPACE=32
# Runtime library

__stdin_buf=
__stdin_line_ends_with_oef=0
_getchar() {
  if [ -z "$__stdin_buf" ] ; then                   # need to get next line when buffer empty
    if [ $__stdin_line_ends_with_oef -eq 1 ]; then  # EOF at end of line, return -1
      : $(($1 = -1))
      __stdin_line_ends_with_oef=0                  # Reset EOF flag for next getchar call
      return
    fi
    IFS=                                            # don't split input
    if read -r __stdin_buf ; then                   # read next line into $__stdin_buf
      if [ -z "$__stdin_buf" ] ; then               # an empty line implies a newline character
        : $(($1 = 10))                              # next getchar call will read next line
        return
      fi
    else
      if [ -z "$__stdin_buf" ] ; then               # EOF reached when read fails
        : $(($1 = -1))
        return
      else
        __stdin_line_ends_with_oef=1
      fi
    fi
  else
    __stdin_buf="${__stdin_buf#?}"                  # remove the current char from $__stdin_buf
    if [ -z "$__stdin_buf" ] ; then                 # end of line if the buffer is now empty
      : $(($1 = 10))
      return
    fi
  fi

  __c=$(LC_CTYPE=C printf "%d" "'${__stdin_buf%"${__stdin_buf#?}"}")
  : $(($1 = __c))
}

# Local variables
__=0
__SP=0
let() { : $((__SP += 1)) $((__$__SP=$1)); } 
endlet() {
  __ret=$1; : $((__tmp = $__ret)) # Save return value so it's not overwritten
  while [ $# -ge 2 ]; do : $(($2 = __$__SP)) $((__SP -= 1)); shift; done
  : $(($__ret=__tmp))
}

_main __

# string_pool_alloc=412 heap_alloc=813 text_alloc=52
