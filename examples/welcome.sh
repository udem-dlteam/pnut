set -e -u

#################################### C code ####################################
# // ask the user to type their name
# void main() {
#   char name[100];
#   int i = 0;
#   puts("What is your name?\n");
#   while ((name[i] = getchar()) != -1 && name[i] != '\n') i += 1;
#   name[i] = '\0';
#   printf("Hello, %s\n", name);
# }
################################# End of C code ################################
: $((__g1 = i = name = 0))
_main() {
  let name; let i; let __g1
  name=0
  i=0
  printf "What is your name?\n"
  while _getchar __g1 ; [ $((_$((name+i)) = __g1)) != -1 ] && [ $((_$((name+i)))) != $__CH_NEWLINE ] ; do
    : $((i += 1))
  done
  : $(( _$((name+i)) = __CH_NULL ))
  printf "Hello, " 
  _put_pstr __ $name
  printf "\n" 
  endlet $1 __g1 i name
}

# Character constants
readonly __CH_NULL=0
readonly __CH_NEWLINE=10
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

_put_pstr() {
  : $(($1 = 0)); shift # Return 0
  __addr=$1; shift
  while [ $(( _$__addr )) -ne 0 ]; do
    printf \\$((_$__addr/64))$((_$__addr/8%8))$((_$__addr%8))
    : $(( __addr += 1 ))
  done
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

# string_pool_alloc=389 heap_alloc=560 text_alloc=41
