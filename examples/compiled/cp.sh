#!/bin/sh
set -e -u -f
LC_ALL=C

: $((filename = 0))
_file_error() { let filename $2
  printf "cp: "
  _put_pstr __ $filename
  printf ": no such file or directory\n"
  exit 1
  endlet $1 filename
}

__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $(($1 = $__ALLOC + 1))
  : $((__ALLOC += $2 + 1))
}

defarr() { _malloc $1 $2; }

defarr _buffer 1024
: $((len = c = dst = src = args = argc = 0))
_main() { let argc $2; let args $3
  let src; let dst; let c; let len
  if [ $argc != 3 ] ; then
    printf "Usage: cp <source> <destination>\n"
    : $(($1 = 1))
    endlet $1 len c dst src args argc
    return
  fi
  _open src $((_$((args + 1)))) 0
  _open dst $((_$((args + 2)))) 1
  if [ $src = 0 ] ; then
    _file_error __ $((_$((args + 1))))
  fi
  if [ $dst = 0 ] ; then
    _file_error __ $((_$((args + 2))))
  fi
  while _read len $src $_buffer 1024; [ $len != 0 ]; do
    _write __ $dst $_buffer $len
  done
  endlet $1 len c dst src args argc
}

#_ Runtime library
_put_pstr() {
  : $(($1 = 0)); shift # Return 0
  __addr=$1; shift
  while [ $((__c = _$__addr)) != 0 ]; do
    printf \\$((__c/64))$((__c/8%8))$((__c%8))
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


#_ Unpack a Shell string into an appropriately sized buffer
unpack_string_to_buf() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?
  __fgetc_buf=$1
  __buffer=$2
  __ends_with_eof=${3:-1}
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

refill_buffer() { # $1: fd
  __fd=$1
  __buffer=$((__buffer_fd$__fd))

  IFS=
  __ends_with_eof=0
  read -r __temp_buf <&$__fd || __ends_with_eof=1

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
  unpack_string_to_buf "$__temp_buf" $__buffer $__ends_with_eof
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

_malloc __buffer_fd0 1000   # Allocate buffer
: $((_$__buffer_fd0 = 0))   # Init buffer to ""
: $((__cursor_fd0 = 0))     # Make buffer empty
: $((__buflen_fd0 = 1000))  # Init buffer length
__state_fd0=0 # stdin
__state_fd1=1 # stdout
__state_fd2=2 # stderr
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
    echo "No more file descriptors available to open $(_put_pstr __ $2)" ; exit 1
  else
    # Because the file must be read line-by-line, and string
    # values can't be assigned to dynamic variables, each line
    # is read and then unpacked in the buffer.
    _malloc __addr 1000                 # Allocate buffer
    : $((_$__addr = 0))                 # Init buffer to ""
    : $((__buffer_fd$__fd = __addr))    # Buffer address
    : $((__cursor_fd$__fd = 0))         # Buffer cursor
    : $((__buflen_fd$__fd = 1000))      # Buffer length
    : $((__state_fd$__fd = $3))         # Mark the fd as opened
    __res=$(_put_pstr __ $2)
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
      echo "Unknown file mode" ; exit 1
    fi
  fi
  : $(($1 = __fd))
}

_read() { : $((__fd = $2)) $((__buf = $3)) $((__count = $4))
  : $((__i = 0))
  while [ $__i -lt $__count ] ; do
    read_byte __byte $__fd
    if [ $__byte -lt 0 ] ; then break; fi
    : $((_$((__buf + __i)) = __byte))
    : $((__i += 1))
  done
  : $(($1 = __i))
}

_write() { : $((__fd = $2)) $((__buf = $3)) $((__count = $4))
  : $((__i = 0))
  while [ $__i -lt $__count ] ; do
    : $((__byte = _$((__buf+__i))))
    printf \\$(($__byte/64))$(($__byte/8%8))$(($__byte%8)) >&$__fd
    : $((__i += 1))
  done
  : $(($1 = __count))
}

make_argv() {
  __argc=$1; shift;
  _malloc __argv $__argc # Allocate enough space for all elements.
  __argv_ptr=$__argv

  while [ $# -ge 1 ]; do
    _malloc _$__argv_ptr $((${#1} + 1))
    unpack_string_to_buf "$1" $((_$__argv_ptr)) 1
    : $((__argv_ptr += 1))
    shift
  done
}

#_ Local variables
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
