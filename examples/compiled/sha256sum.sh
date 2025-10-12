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

defarr _k 64
_sha256_setup() {
  : $((_$((_k + 0)) = 0x428a2f98))
  : $((_$((_k + 1)) = 0x71374491))
  : $((_$((_k + 2)) = 0xb5c0fbcf))
  : $((_$((_k + 3)) = 0xe9b5dba5))
  : $((_$((_k + 4)) = 0x3956c25b))
  : $((_$((_k + 5)) = 0x59f111f1))
  : $((_$((_k + 6)) = 0x923f82a4))
  : $((_$((_k + 7)) = 0xab1c5ed5))
  : $((_$((_k + 8)) = 0xd807aa98))
  : $((_$((_k + 9)) = 0x12835b01))
  : $((_$((_k + 10)) = 0x243185be))
  : $((_$((_k + 11)) = 0x550c7dc3))
  : $((_$((_k + 12)) = 0x72be5d74))
  : $((_$((_k + 13)) = 0x80deb1fe))
  : $((_$((_k + 14)) = 0x9bdc06a7))
  : $((_$((_k + 15)) = 0xc19bf174))
  : $((_$((_k + 16)) = 0xe49b69c1))
  : $((_$((_k + 17)) = 0xefbe4786))
  : $((_$((_k + 18)) = 0xfc19dc6))
  : $((_$((_k + 19)) = 0x240ca1cc))
  : $((_$((_k + 20)) = 0x2de92c6f))
  : $((_$((_k + 21)) = 0x4a7484aa))
  : $((_$((_k + 22)) = 0x5cb0a9dc))
  : $((_$((_k + 23)) = 0x76f988da))
  : $((_$((_k + 24)) = 0x983e5152))
  : $((_$((_k + 25)) = 0xa831c66d))
  : $((_$((_k + 26)) = 0xb00327c8))
  : $((_$((_k + 27)) = 0xbf597fc7))
  : $((_$((_k + 28)) = 0xc6e00bf3))
  : $((_$((_k + 29)) = 0xd5a79147))
  : $((_$((_k + 30)) = 0x6ca6351))
  : $((_$((_k + 31)) = 0x14292967))
  : $((_$((_k + 32)) = 0x27b70a85))
  : $((_$((_k + 33)) = 0x2e1b2138))
  : $((_$((_k + 34)) = 0x4d2c6dfc))
  : $((_$((_k + 35)) = 0x53380d13))
  : $((_$((_k + 36)) = 0x650a7354))
  : $((_$((_k + 37)) = 0x766a0abb))
  : $((_$((_k + 38)) = 0x81c2c92e))
  : $((_$((_k + 39)) = 0x92722c85))
  : $((_$((_k + 40)) = 0xa2bfe8a1))
  : $((_$((_k + 41)) = 0xa81a664b))
  : $((_$((_k + 42)) = 0xc24b8b70))
  : $((_$((_k + 43)) = 0xc76c51a3))
  : $((_$((_k + 44)) = 0xd192e819))
  : $((_$((_k + 45)) = 0xd6990624))
  : $((_$((_k + 46)) = 0xf40e3585))
  : $((_$((_k + 47)) = 0x106aa070))
  : $((_$((_k + 48)) = 0x19a4c116))
  : $((_$((_k + 49)) = 0x1e376c08))
  : $((_$((_k + 50)) = 0x2748774c))
  : $((_$((_k + 51)) = 0x34b0bcb5))
  : $((_$((_k + 52)) = 0x391c0cb3))
  : $((_$((_k + 53)) = 0x4ed8aa4a))
  : $((_$((_k + 54)) = 0x5b9cca4f))
  : $((_$((_k + 55)) = 0x682e6ff3))
  : $((_$((_k + 56)) = 0x748f82ee))
  : $((_$((_k + 57)) = 0x78a5636f))
  : $((_$((_k + 58)) = 0x84c87814))
  : $((_$((_k + 59)) = 0x8cc70208))
  : $((_$((_k + 60)) = 0x90befffa))
  : $((_$((_k + 61)) = 0xa4506ceb))
  : $((_$((_k + 62)) = 0xbef9a3f7))
  : $((_$((_k + 63)) = 0xc67178f2))
}

defarr _w 64
_nbits=0
defarr _hash 8
defarr _temp 8
_sha256_init() {
  _nbits=0
  : $((_$((_hash + 0)) = 0x6a09e667))
  : $((_$((_hash + 1)) = 0xbb67ae85))
  : $((_$((_hash + 2)) = 0x3c6ef372))
  : $((_$((_hash + 3)) = 0xa54ff53a))
  : $((_$((_hash + 4)) = 0x510e527f))
  : $((_$((_hash + 5)) = 0x9b05688c))
  : $((_$((_hash + 6)) = 0x1f83d9ab))
  : $((_$((_hash + 7)) = 0x5be0cd19))
}

: $((t2 = ma = t1 = ch = i = s1 = s0 = b3 = b2 = b1 = b0 = bytes = 0))
_sha256_add_block() { let bytes $2
  let b0; let b1; let b2; let b3; let s0; let s1; let i; let ch; let t1; let ma; let t2
  i=0
  while [ $i -lt 16 ]; do
    b0=$((0xff & _$((bytes + (i * 4)))))
    b1=$((0xff & _$((bytes + (i * 4) + 1))))
    b2=$((0xff & _$((bytes + (i * 4) + 2))))
    b3=$((0xff & _$((bytes + (i * 4) + 3))))
    : $((_$((_w + i)) = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3))
    : $((i += 1))
  done
  i=16
  while [ $i -lt 64 ]; do
    s0=$(((((_$((_w + (i - 15))) >> 7) & (0x7fffffff >> (7 - 1))) | ((_$((_w + (i - 15))) << (32 - 7)) & 0xffffffff)) ^ (((_$((_w + (i - 15))) >> 18) & (0x7fffffff >> (18 - 1))) | ((_$((_w + (i - 15))) << (32 - 18)) & 0xffffffff)) ^ ((_$((_w + (i - 15))) >> 3) & 0x1fffffff)))
    s1=$(((((_$((_w + (i - 2))) >> 17) & (0x7fffffff >> (17 - 1))) | ((_$((_w + (i - 2))) << (32 - 17)) & 0xffffffff)) ^ (((_$((_w + (i - 2))) >> 19) & (0x7fffffff >> (19 - 1))) | ((_$((_w + (i - 2))) << (32 - 19)) & 0xffffffff)) ^ ((_$((_w + (i - 2))) >> 10) & 0x3fffff)))
    : $((_$((_w + i)) = (_$((_w + (i - 16))) + s0 + _$((_w + (i - 7))) + s1) & 0xffffffff))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 8 ]; do
    : $((_$((_temp + i)) = _$((_hash + i))))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 64 ]; do
    s1=$(((((_$((_temp + 4)) >> 6) & (0x7fffffff >> (6 - 1))) | ((_$((_temp + 4)) << (32 - 6)) & 0xffffffff)) ^ (((_$((_temp + 4)) >> 11) & (0x7fffffff >> (11 - 1))) | ((_$((_temp + 4)) << (32 - 11)) & 0xffffffff)) ^ (((_$((_temp + 4)) >> 25) & (0x7fffffff >> (25 - 1))) | ((_$((_temp + 4)) << (32 - 25)) & 0xffffffff))))
    ch=$(((_$((_temp + 4)) & _$((_temp + 5))) ^ (~_$((_temp + 4)) & _$((_temp + 6)))))
    t1=$(((_$((_temp + 7)) + s1 + ch + _$((_k + i)) + _$((_w + i))) & 0xffffffff))
    s0=$(((((_$((_temp + 0)) >> 2) & (0x7fffffff >> (2 - 1))) | ((_$((_temp + 0)) << (32 - 2)) & 0xffffffff)) ^ (((_$((_temp + 0)) >> 13) & (0x7fffffff >> (13 - 1))) | ((_$((_temp + 0)) << (32 - 13)) & 0xffffffff)) ^ (((_$((_temp + 0)) >> 22) & (0x7fffffff >> (22 - 1))) | ((_$((_temp + 0)) << (32 - 22)) & 0xffffffff))))
    ma=$(((_$((_temp + 0)) & _$((_temp + 1))) ^ (_$((_temp + 0)) & _$((_temp + 2))) ^ (_$((_temp + 1)) & _$((_temp + 2)))))
    t2=$(((s0 + ma) & 0xffffffff))
    : $((_$((_temp + 7)) = _$((_temp + 6))))
    : $((_$((_temp + 6)) = _$((_temp + 5))))
    : $((_$((_temp + 5)) = _$((_temp + 4))))
    : $((_$((_temp + 4)) = (_$((_temp + 3)) + t1) & 0xffffffff))
    : $((_$((_temp + 3)) = _$((_temp + 2))))
    : $((_$((_temp + 2)) = _$((_temp + 1))))
    : $((_$((_temp + 1)) = _$((_temp + 0))))
    : $((_$((_temp + 0)) = (t1 + t2) & 0xffffffff))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 8 ]; do
    : $((_$((_hash + i)) = (_$((_hash + i)) + _$((_temp + i))) & 0xffffffff))
    : $((i += 1))
  done
  endlet $1 t2 ma t1 ch i s1 s0 b3 b2 b1 b0 bytes
}

defarr _buf 64
: $((digits = byte = 0))
_hex() { let byte $2
  let digits
  defstr __str_0 "0123456789abcdef"
  digits=$__str_0
  printf \\$(((_$((digits + (0xf & (byte >> 4)))))/64))$(((_$((digits + (0xf & (byte >> 4)))))/8%8))$(((_$((digits + (0xf & (byte >> 4)))))%8))
  printf \\$(((_$((digits + (0xf & byte))))/64))$(((_$((digits + (0xf & byte))))/8%8))$(((_$((digits + (0xf & byte))))%8))
  endlet $1 digits byte
}

: $((h = n = fd = i = filename = 0))
_process_file() { let filename $2
  let i; let fd; let n; let h
  n=64
  _sha256_setup __
  _sha256_init __
  _open fd $filename 0
  while [ $n = 64 ]; do
    _read n $fd $_buf 64
    if [ $n -lt 0 ] ; then
      : $(($1 = 1))
      endlet $1 h n fd i filename
      return
    fi
    : $((_nbits += (8 * n)))
    if [ $n -lt 64 ] ; then
      : $((_$((_buf + n)) = 0x80))
      i=$((n + 1))
      while [ $i -lt 64 ]; do
        : $((_$((_buf + i)) = 0))
        : $((i += 1))
      done
      if [ $n -ge $((64 - 9)) ] ; then
        _sha256_add_block __ $_buf
        i=0
        while [ $i -lt $((64 - 8)) ]; do
          : $((_$((_buf + i)) = 0))
          : $((i += 1))
        done
      fi
      i=1
      while [ $i -le 8 ]; do
        : $((_$((_buf + 64 - i)) = 0xff & _nbits))
        : $((_nbits >>= 8))
        : $((i += 1))
      done
    fi
    _sha256_add_block __ $_buf
  done
  _close __ $fd
  i=0
  while [ $i -lt 8 ]; do
    h=$((_$((_hash + i))))
    _hex __ $((h >> 24))
    _hex __ $((h >> 16))
    _hex __ $((h >> 8))
    _hex __ $h
    : $((i += 1))
  done
  printf " "
  printf " "
  while [ $((_$filename)) != 0 ]; do
    printf \\$(((_$filename)/64))$(((_$filename)/8%8))$(((_$filename)%8))
    : $((filename += 1))
  done
  printf "\n"
  : $(($1 = 0))
  endlet $1 h n fd i filename
}

: $((__t1 = i = myargv = argc = 0))
_main() { let argc $2; let myargv $3
  let i; let __t1
  i=1
  while [ $i -lt $argc ]; do
    if _process_file __t1 $((_$((myargv + i)))); [ $__t1 != 0 ] ; then
      break
    fi
    : $((i += 1))
  done
  : $(($1 = 0))
  endlet $1 __t1 i myargv argc
}

# Runtime library

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
  unpack_string "$__temp_buf" $__buffer $__ends_with_eof
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

_put_pstr() {
  : $(($1 = 0)); shift # Return 0
  __addr=$1; shift
  while [ $((__c = _$__addr)) != 0 ]; do
    printf \\$((__c/64))$((__c/8%8))$((__c%8))
    : $((__addr += 1))
  done
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
