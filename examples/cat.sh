set -e -u

#################################### C code ####################################
# // Utility in C that reads a file and does nothing with it
# void main(int argc, char **argv) {
#   int fd;
#   int c;
#   fd = fopen(argv[1], "r");
#   while ((c = fgetc(fd)) != -1) {
#     putchar(c);
#   }
# }
################################# End of C code ################################
: $((__g1 = c = fd = argv_ = argc = 0))
_main() { # argc: $2, argv: $3
  let argc; let argv_; let fd; let c; let __g1
  argc=$2
  argv_=$3
  fd=0
  c=0
  defstr __str_0 "r"
  _fopen fd $((_$((argv_ + 1)))) $__str_0
  while _fgetc __g1 $fd; [ $((c = __g1)) != -1 ] ; do
    printf \\$(($c/64))$(($c/8%8))$(($c%8))
  done
  endlet $1 __g1 c fd argv_ argc
}

# Runtime library
__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $((__ALLOC += 1))
  : $(($1 = $__ALLOC))
  : $((__ALLOC += $2))
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
        __c=$(LC_CTYPE=C printf "%d" "'${__buf%"${__buf#?}"}")
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


# Convert a VM string reference to a Shell string.
# $__res is set to the result, and $__len is set to the length of the string.
pack_string() {
  __addr=$1; shift
  __max_len=100000000
  __delim=0
  __len=0
  __res=""
  if [ $# -ge 1 ] ; then __delim=$1   ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then __max_len=$1 ; shift ; fi # Optional max length
  while [ $((_$__addr)) != $__delim ] && [ $__max_len -gt $__len ] ; do
    __char=$((_$__addr))
    __addr=$((__addr + 1))
    __len=$((__len + 1))
    case $__char in
      10) __res="$__res\n" ;; # 10 == '\n'
      *)        __char=$(printf "\\$(printf "%o" "$__char")")
        __res="$__res$__char" ;;
    esac
  done
}

__state_fd0=0;
_malloc __buffer_fd0 1000   # Allocate buffer
: $((_$__buffer_fd0 = 0))   # Init buffer to ""
: $((__cursor_fd0 = 0))     # Make buffer empty
: $((__buflen_fd0 = 1000))  # Init buffer length
__state_fd1=1
__state_fd2=1
__state_fd3=-1
__state_fd4=-1
__state_fd5=-1
__state_fd6=-1
__state_fd7=-1
__state_fd8=-1
__state_fd9=-1

_open() { # $2: filename, $3: flags, $4: mode
  # Get available fd
  __fd=0
  while [ $__fd -lt 10 ]; do
    if [ $((__state_fd$__fd)) -lt 0 ]; then
      break
    fi
    : $((__fd += 1))
  done
  if [ $__fd -gt 9 ] ; then
    # Some shells don't support fd > 9
    echo "No more file descriptors available" ; exit 1
  else
    # Because the file must be read line-by-line, and string
    # values can't be assigned to dynamic variables, each line
    # is read and then unpacked in the buffer.
    _malloc __addr 1000                   # Allocate buffer
    : $((_$__addr = 0))                 # Init buffer to ""
    : $((__buffer_fd$__fd = __addr))    # Save buffer address
    : $((__cursor_fd$__fd = 0))         # Make buffer empty
    : $((__buflen_fd$__fd = 1000))      # Init buffer length
    : $((__state_fd$__fd = $3))         # Mark the fd as opened
    pack_string $2
    if [ $3 = 0 ] ; then
      case $__fd in
        0) exec 0< $__res ;; 1) exec 1< $__res ;; 2) exec 2< $__res ;;
        3) exec 3< $__res ;; 4) exec 4< $__res ;; 5) exec 5< $__res ;;
        6) exec 6< $__res ;; 7) exec 7< $__res ;; 8) exec 8< $__res ;;
        9) exec 9< $__res ;;
      esac
    elif [ $3 = 1 ] ; then
      case $__fd in
        0) exec 0> $__res ;; 1) exec 1> $__res ;; 2) exec 2> $__res ;;
        3) exec 3> $__res ;; 4) exec 4> $__res ;; 5) exec 5> $__res ;;
        6) exec 6> $__res ;; 7) exec 7> $__res ;; 8) exec 8> $__res ;;
        9) exec 9> $__res ;;
      esac
    elif [ $3 = 2 ] ; then
      case $__fd in
        0) exec 0>> $__res ;; 1) exec 1>> $__res ;; 2) exec 2>> $__res ;;
        3) exec 3>> $__res ;; 4) exec 4>> $__res ;; 5) exec 5>> $__res ;;
        6) exec 6>> $__res ;; 7) exec 7>> $__res ;; 8) exec 8>> $__res ;;
        9) exec 9>> $__res ;;
      esac
    else
      echo "Unknow file mode" ; exit 1
    fi
  fi
  : $(($1 = __fd))
}

# Open the file and return a FILE* for the file.
# The FILE structure contains the file descriptor.
_fopen() { # $2: File name, $3: Mode
  _open __fd $2 $((_$3 == 119)) 511
  _malloc __file 1        # Allocate FILE structure
  : $((_$__file = __fd))  # Save fd
  : $(($1 = __file))
}

_free() { # $1 = pointer to object to free
  : $(($1 = 0)); shift # Return 0
  __ptr=$1
  __size=$((_$((__ptr - 1)))) # Get size of allocation
  while [ $__size -gt 0 ]; do
    unset "_$__ptr"
    : $((__ptr += 1))
    : $((__size -= 1))
  done
}

# Unpack a Shell string into an appropriately sized buffer
unpack_line() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?
  __fgetc_buf=$1
  __buffer=$2
  __ends_with_eof=$3
  __fgetc_buf32=
  __fgetc_buf64=
  __fgetc_buf128=
  __continue=1
  while [ $__continue != 0 ] ; do
    if [ -z "$__fgetc_buf128" ]; then
      if [ ${#__fgetc_buf} -ge 128 ]; then
        __temp=${__fgetc_buf#????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????}
        __fgetc_buf128="${__fgetc_buf%"$__temp"}"
        __fgetc_buf=$__temp
      else
        __fgetc_buf128=$__fgetc_buf
        __fgetc_buf=
      fi
    fi
    if [ -z "$__fgetc_buf64" ]; then
      if [ ${#__fgetc_buf128} -ge 64 ]; then
        __temp=${__fgetc_buf128#????????????????????????????????????????????????????????????????}
        __fgetc_buf64="${__fgetc_buf128%"$__temp"}"
        __fgetc_buf128=$__temp
      else
        __fgetc_buf64=$__fgetc_buf128
        __fgetc_buf128=
      fi
    fi
    if [ -z "$__fgetc_buf32" ]; then
      if [ ${#__fgetc_buf64} -ge 32 ]; then
        __temp=${__fgetc_buf64#????????????????????????????????}
        __fgetc_buf32="${__fgetc_buf64%"$__temp"}"
        __fgetc_buf64=$__temp
      else
        __fgetc_buf32=$__fgetc_buf64
        __fgetc_buf64=
        __continue=0
      fi
    fi
    while [ ! -z "$__fgetc_buf32" ]; do
      __c=$(LC_CTYPE=C printf "%d" "'${__fgetc_buf32%"${__fgetc_buf32#?}"}")
      : $((_$__buffer = __c))
      __fgetc_buf32=${__fgetc_buf32#?}  # Remove the first character
      : $((__buffer += 1))              # Move to the next buffer position
    done
  done

  if [ $__ends_with_eof -eq 0 ]; then # Ends with newline and not EOF?
    : $((_$__buffer = 10))            # Line ends with newline
    : $((__buffer += 1))
  fi
  : $((_$__buffer = 0))               # Then \0
}

refill_buffer() { # $1: fd
  __fd=$1
  __buffer=$((__buffer_fd$__fd))

  IFS=
  if read -r __temp_buf <&$__fd ; then  # read next line into $__temp_buf
    __ends_with_eof=0
  else
    __ends_with_eof=1
  fi

  # Check that the buffer is large enough to unpack the line
  __buflen=$((__buflen_fd$__fd - 2)) # Minus 2 to account for newline and \0
  __len=${#__temp_buf}
  if [ $__len -gt $__buflen ]; then
    # Free buffer and reallocate a new one double the line size
    __buflen=$((__len * 2))
    _free __ $__buffer
    _malloc __buffer $__buflen
    : $((__buffer_fd$__fd = __buffer))
    : $((__buflen_fd$__fd = __buflen))
  fi
  unpack_line "$__temp_buf" $__buffer $__ends_with_eof
}

read_byte() { # $2: fd
  __fd=$2
  : $((__buffer=__buffer_fd$__fd))
  : $((__cursor=__cursor_fd$__fd))
  # The cursor is at the end of the buffer, we need to read the next line
  if [ $((_$((__buffer + __cursor)))) -eq 0 ]; then
    # Buffer has been read completely, read next line
    refill_buffer $__fd
    __cursor=0 # Reset cursor and reload buffer
    : $((__buffer=__buffer_fd$__fd))
    if [ $((_$((__buffer + __cursor)))) -eq 0 ]; then
      : $(($1 = -1)) # EOF
      return
    fi
  fi
  : $(($1 = _$((__buffer + __cursor))))
  : $((__cursor_fd$__fd = __cursor + 1))  # Increment cursor
}

_fgetc() { # $2: file
  __file=$2
  __fd=$((_$__file))
  read_byte $1 $__fd
}

# Push a Shell string to the VM heap. Returns a reference to the string in $__addr.
unpack_string() {
  __buf="$1"
  _malloc __addr $((${#__buf} + 1))
  __ptr=$__addr
  while [ -n "$__buf" ] ; do
    __char="${__buf%"${__buf#?}"}"   # remove all but first char
    __buf="${__buf#?}"               # remove the current char from $__buf
    __c=$(LC_CTYPE=C printf "%d" "'$__char")
    : $((_$__ptr = __c))
    : $((__ptr += 1))
  done
  : $((_$__ptr = 0))
}

make_argv() {
  __argc=$1; shift;
  _malloc __argv $__argc # Allocate enough space for all elements. No need to initialize.
  __argv_ptr=$__argv     # __ptr is used by unpack_string

  while [ $# -ge 1 ]; do
    unpack_string "$1"
    : $((_$__argv_ptr = $__addr))
    : $((__argv_ptr += 1))
    shift
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

# Setup argc, argv
__argc_for_main=$(($# + 1))
make_argv $__argc_for_main "$0" $@; __argv_for_main=$__argv
_main __ $__argc_for_main $__argv_for_main

# string_pool_alloc=391 heap_alloc=597 max_text_alloc=687 cumul_text_alloc=687
