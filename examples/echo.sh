set -e -u

#################################### C code ####################################
# // echo program
# void main() {
#   char c;
#   while ((c = getchar()) != -1) {
#     putchar(c);
#   }
# }
################################# End of C code ################################
: $((__g1 = c = 0))
_main() {
  let c; let __g1
  c=0
  while _getchar __g1 ; [ $((c = __g1)) != -1 ] ; do
    printf \\$(($c/64))$(($c/8%8))$(($c%8))
  done
  endlet $1 __g1 c
}

# Runtime library

__stdin_buf=
__stdin_line_ends_with_eof=0
_getchar() {
  if [ -z "$__stdin_buf" ] ; then                   # need to get next line when buffer empty
    if [ $__stdin_line_ends_with_eof -eq 1 ]; then  # EOF at end of line, return -1
      : $(($1 = -1))
      __stdin_line_ends_with_eof=0                  # Reset EOF flag for next getchar call
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
        __stdin_line_ends_with_eof=1
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

# string_pool_alloc=375 heap_alloc=453 max_text_alloc=360 cumul_text_alloc=360
