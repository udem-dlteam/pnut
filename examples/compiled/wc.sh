#!/bin/sh
set -e -u

__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $(($1 = $__ALLOC + 1))
  : $((__ALLOC += $2 + 1))
}

defarr() { _malloc $1 $2; }

defarr _buf 1024
: $((c = 0))
_is_word_separator() { let c $2
  : $(($1 = (c == __SPACE__) || (c == __NEWLINE__) || (c == __TAB__)))
  endlet $1 c
}

: $((n = i = last_sep = sep = chars = words = lines = filename = fd = 0))
_wc_fd() { let fd $2; let filename $3
  let lines; let words; let chars; let sep; let last_sep; let i; let n
  lines=0
  words=0
  chars=0
  sep=0
  last_sep=0
  n=1024
  while [ $n = 1024 ] ; do
    _read n $fd $_buf 1024
    i=0
    while [ $i -lt $n ] ; do
      : $((chars += 1))
      if [ $((_$((_buf + i)))) = $__NEWLINE__ ] ; then
        : $((lines += 1))
      fi
      _is_word_separator sep $((_$((_buf + i))))
      if [ $sep != 0 ] && [ $((!last_sep)) != 0 ] ; then
        : $((words += 1))
      fi
      last_sep=$sep
      : $((i += 1))
    done
  done
  if [ $filename != 0 ] ; then
    printf "%d %d %d " $lines $words $chars
    _put_pstr __ $filename
    printf "\n" 
  else
    printf "%d %d %d\n" $lines $words $chars
  fi
  endlet $1 n i last_sep sep chars words lines filename fd
}

: $((fd = filename = 0))
_wc_file() { let filename $2
  let fd
  _open fd $filename 0
  if [ $fd -lt 0 ] ; then
    exit 1
  fi
  _wc_fd __ $fd $filename
  _close __ $fd
  endlet $1 fd filename
}

: $((i = argv_ = argc = 0))
_main() { let argc $2; let argv_ $3
  let i
  if [ $argc -ge 2 ] ; then
    i=1
    while [ $i -lt $argc ] ; do
      _wc_file __ $((_$((argv_ + i))))
      : $((i += 1))
    done
  else
    _wc_fd __ 0 0
  fi
  : $(($1 = 0))
  endlet $1 i argv_ argc
}

# Character constants
readonly __TAB__=9
readonly __NEWLINE__=10
readonly __SPACE__=32
# Runtime library
_put_pstr() {
  : $(($1 = 0)); shift # Return 0
  __addr=$1; shift
  while [ $((_$__addr)) != 0 ]; do
    printf "%b" "\0$((_$__addr/64))$((_$__addr/8%8))$((_$__addr%8))"
    : $((__addr += 1))
  done
}

_free() { # $2 = object to free
  __ptr=$(($2 - 1))          # Start of object
  __end=$((__ptr + _$__ptr)) # End of object
  while [ $__ptr -lt $__end ]; do
    unset "_$__ptr"
    : $((__ptr += 1))
  done
  : $(($1 = 0))              # Return 0
}


# Unpack a Shell string into an appropriately sized buffer
unpack_line() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?
  __fgetc_buf=$1
  __buffer=$2
  __ends_with_eof=$3
  while [ ! -z "$__fgetc_buf" ]; do
    __c=$(LC_CTYPE=C printf "%d" "'${__fgetc_buf%"${__fgetc_buf#?}"}")
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
      *)        __char=$(printf "\\$(($__char/64))$(($__char/8%8))$(($__char%8))")
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
        0) exec 0< "$__res" ;; 1) exec 1< "$__res" ;; 2) exec 2< "$__res" ;;
        3) exec 3< "$__res" ;; 4) exec 4< "$__res" ;; 5) exec 5< "$__res" ;;
        6) exec 6< "$__res" ;; 7) exec 7< "$__res" ;; 8) exec 8< "$__res" ;;
        9) exec 9< "$__res" ;;
      esac
    elif [ $3 = 1 ] ; then
      case $__fd in
        0) exec 0> "$__res" ;; 1) exec 1> "$__res" ;; 2) exec 2> "$__res" ;;
        3) exec 3> "$__res" ;; 4) exec 4> "$__res" ;; 5) exec 5> "$__res" ;;
        6) exec 6> "$__res" ;; 7) exec 7> "$__res" ;; 8) exec 8> "$__res" ;;
        9) exec 9> "$__res" ;;
      esac
    elif [ $3 = 2 ] ; then
      case $__fd in
        0) exec 0>> "$__res" ;; 1) exec 1>> "$__res" ;; 2) exec 2>> "$__res" ;;
        3) exec 3>> "$__res" ;; 4) exec 4>> "$__res" ;; 5) exec 5>> "$__res" ;;
        6) exec 6>> "$__res" ;; 7) exec 7>> "$__res" ;; 8) exec 8>> "$__res" ;;
        9) exec 9>> "$__res" ;;
      esac
    else
      echo "Unknow file mode" ; exit 1
    fi
  fi
  : $(($1 = __fd))
}

_read() { : $((__fd = $2)) $((__buf = $3)) $((__count = $4))
  : $((__i = 0))
  while [ $__i -lt $__count ] ; do
    read_byte __byte $__fd
    if [ $__byte -lt 0 ] ; then
      break
    fi
    : $((_$((__buf + __i)) = __byte))
    : $((__i += 1))
  done
  : $(($1 = __i))
}

_close() { # $2: fd
  __fd=$2
  __buf=$((__buffer_fd$__fd))  # Get buffer
  _free __ $__buf              # Release buffer
  : $((__state_fd$__fd = -1))  # Mark the fd as closed
  case $__fd in
    0) exec 0<&- ;; 1) exec 1<&- ;; 2) exec 2<&- ;;
    3) exec 3<&- ;; 4) exec 4<&- ;; 5) exec 5<&- ;;
    6) exec 6<&- ;; 7) exec 7<&- ;; 8) exec 8<&- ;;
    9) exec 9<&- ;;
  esac
  : $(($1 = 0))
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
    __c=$(LC_CTYPE=C printf "%d" "'$__char")
    # Write character to memory
    : $((_$__ptr = __c))
    # Continue with rest of string
    : $((__ptr += 1))
    __str="$__tail"
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
__code=0; # Success exit code
_main __code $__argc_for_main $__argv_for_main; exit $__code
