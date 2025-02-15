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
  : $((_$((_k + 0)) = (17034 << 16) | 12184))
  : $((_$((_k + 1)) = (28983 << 16) | 17553))
  : $((_$((_k + 2)) = (46528 << 16) | 64463))
  : $((_$((_k + 3)) = (59829 << 16) | 56229))
  : $((_$((_k + 4)) = (14678 << 16) | 49755))
  : $((_$((_k + 5)) = (23025 << 16) | 4593))
  : $((_$((_k + 6)) = (37439 << 16) | 33444))
  : $((_$((_k + 7)) = (43804 << 16) | 24277))
  : $((_$((_k + 8)) = (55303 << 16) | 43672))
  : $((_$((_k + 9)) = (4739 << 16) | 23297))
  : $((_$((_k + 10)) = (9265 << 16) | 34238))
  : $((_$((_k + 11)) = (21772 << 16) | 32195))
  : $((_$((_k + 12)) = (29374 << 16) | 23924))
  : $((_$((_k + 13)) = (32990 << 16) | 45566))
  : $((_$((_k + 14)) = (39900 << 16) | 1703))
  : $((_$((_k + 15)) = (49563 << 16) | 61812))
  : $((_$((_k + 16)) = (58523 << 16) | 27073))
  : $((_$((_k + 17)) = (61374 << 16) | 18310))
  : $((_$((_k + 18)) = (4033 << 16) | 40390))
  : $((_$((_k + 19)) = (9228 << 16) | 41420))
  : $((_$((_k + 20)) = (11753 << 16) | 11375))
  : $((_$((_k + 21)) = (19060 << 16) | 33962))
  : $((_$((_k + 22)) = (23728 << 16) | 43484))
  : $((_$((_k + 23)) = (30457 << 16) | 35034))
  : $((_$((_k + 24)) = (38974 << 16) | 20818))
  : $((_$((_k + 25)) = (43057 << 16) | 50797))
  : $((_$((_k + 26)) = (45059 << 16) | 10184))
  : $((_$((_k + 27)) = (48985 << 16) | 32711))
  : $((_$((_k + 28)) = (50912 << 16) | 3059))
  : $((_$((_k + 29)) = (54695 << 16) | 37191))
  : $((_$((_k + 30)) = (1738 << 16) | 25425))
  : $((_$((_k + 31)) = (5161 << 16) | 10599))
  : $((_$((_k + 32)) = (10167 << 16) | 2693))
  : $((_$((_k + 33)) = (11803 << 16) | 8504))
  : $((_$((_k + 34)) = (19756 << 16) | 28156))
  : $((_$((_k + 35)) = (21304 << 16) | 3347))
  : $((_$((_k + 36)) = (25866 << 16) | 29524))
  : $((_$((_k + 37)) = (30314 << 16) | 2747))
  : $((_$((_k + 38)) = (33218 << 16) | 51502))
  : $((_$((_k + 39)) = (37490 << 16) | 11397))
  : $((_$((_k + 40)) = (41663 << 16) | 59553))
  : $((_$((_k + 41)) = (43034 << 16) | 26187))
  : $((_$((_k + 42)) = (49739 << 16) | 35696))
  : $((_$((_k + 43)) = (51052 << 16) | 20899))
  : $((_$((_k + 44)) = (53650 << 16) | 59417))
  : $((_$((_k + 45)) = (54937 << 16) | 1572))
  : $((_$((_k + 46)) = (62478 << 16) | 13701))
  : $((_$((_k + 47)) = (4202 << 16) | 41072))
  : $((_$((_k + 48)) = (6564 << 16) | 49430))
  : $((_$((_k + 49)) = (7735 << 16) | 27656))
  : $((_$((_k + 50)) = (10056 << 16) | 30540))
  : $((_$((_k + 51)) = (13488 << 16) | 48309))
  : $((_$((_k + 52)) = (14620 << 16) | 3251))
  : $((_$((_k + 53)) = (20184 << 16) | 43594))
  : $((_$((_k + 54)) = (23452 << 16) | 51791))
  : $((_$((_k + 55)) = (26670 << 16) | 28659))
  : $((_$((_k + 56)) = (29839 << 16) | 33518))
  : $((_$((_k + 57)) = (30885 << 16) | 25455))
  : $((_$((_k + 58)) = (33992 << 16) | 30740))
  : $((_$((_k + 59)) = (36039 << 16) | 520))
  : $((_$((_k + 60)) = (37054 << 16) | 65530))
  : $((_$((_k + 61)) = (42064 << 16) | 27883))
  : $((_$((_k + 62)) = (48889 << 16) | 41975))
  : $((_$((_k + 63)) = (50801 << 16) | 30962))
}

defarr _w 64
_nbits=0
defarr _hash 8
defarr _temp 8
_sha256_init() {
  _nbits=0
  : $((_$((_hash + 0)) = (27145 << 16) | 58983))
  : $((_$((_hash + 1)) = (47975 << 16) | 44677))
  : $((_$((_hash + 2)) = (15470 << 16) | 62322))
  : $((_$((_hash + 3)) = (42319 << 16) | 62778))
  : $((_$((_hash + 4)) = (20750 << 16) | 21119))
  : $((_$((_hash + 5)) = (39685 << 16) | 26764))
  : $((_$((_hash + 6)) = (8067 << 16) | 55723))
  : $((_$((_hash + 7)) = (23520 << 16) | 52505))
}

: $((t2 = ma = t1 = ch = i = s1 = s0 = b3 = b2 = b1 = b0 = bytes = 0))
_sha256_add_block() { let bytes $2
  let b0; let b1; let b2; let b3; let s0; let s1; let i; let ch; let t1; let ma; let t2
  i=0
  while [ $i -lt 16 ]; do
    b0=$((255 & _$((bytes + (i * 4)))))
    b1=$((255 & _$((bytes + (i * 4) + 1))))
    b2=$((255 & _$((bytes + (i * 4) + 2))))
    b3=$((255 & _$((bytes + (i * 4) + 3))))
    : $((_$((_w + i)) = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3))
    : $((i += 1))
  done
  i=16
  while [ $i -lt 64 ]; do
    s0=$(((((_$((_w + (i - 15))) >> 7) & (2147483647 >> (7 - 1))) | ((_$((_w + (i - 15))) << (32 - 7)) & ((65535 << 16) | 65535))) ^ (((_$((_w + (i - 15))) >> 18) & (2147483647 >> (18 - 1))) | ((_$((_w + (i - 15))) << (32 - 18)) & ((65535 << 16) | 65535))) ^ ((_$((_w + (i - 15))) >> 3) & 536870911)))
    s1=$(((((_$((_w + (i - 2))) >> 17) & (2147483647 >> (17 - 1))) | ((_$((_w + (i - 2))) << (32 - 17)) & ((65535 << 16) | 65535))) ^ (((_$((_w + (i - 2))) >> 19) & (2147483647 >> (19 - 1))) | ((_$((_w + (i - 2))) << (32 - 19)) & ((65535 << 16) | 65535))) ^ ((_$((_w + (i - 2))) >> 10) & 4194303)))
    : $((_$((_w + i)) = (_$((_w + (i - 16))) + s0 + _$((_w + (i - 7))) + s1) & ((65535 << 16) | 65535)))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 8 ]; do
    : $((_$((_temp + i)) = _$((_hash + i))))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 64 ]; do
    s1=$(((((_$((_temp + 4)) >> 6) & (2147483647 >> (6 - 1))) | ((_$((_temp + 4)) << (32 - 6)) & ((65535 << 16) | 65535))) ^ (((_$((_temp + 4)) >> 11) & (2147483647 >> (11 - 1))) | ((_$((_temp + 4)) << (32 - 11)) & ((65535 << 16) | 65535))) ^ (((_$((_temp + 4)) >> 25) & (2147483647 >> (25 - 1))) | ((_$((_temp + 4)) << (32 - 25)) & ((65535 << 16) | 65535)))))
    ch=$(((_$((_temp + 4)) & _$((_temp + 5))) ^ (~(_$((_temp + 4))) & _$((_temp + 6)))))
    t1=$(((_$((_temp + 7)) + s1 + ch + _$((_k + i)) + _$((_w + i))) & ((65535 << 16) | 65535)))
    s0=$(((((_$((_temp + 0)) >> 2) & (2147483647 >> (2 - 1))) | ((_$((_temp + 0)) << (32 - 2)) & ((65535 << 16) | 65535))) ^ (((_$((_temp + 0)) >> 13) & (2147483647 >> (13 - 1))) | ((_$((_temp + 0)) << (32 - 13)) & ((65535 << 16) | 65535))) ^ (((_$((_temp + 0)) >> 22) & (2147483647 >> (22 - 1))) | ((_$((_temp + 0)) << (32 - 22)) & ((65535 << 16) | 65535)))))
    ma=$(((_$((_temp + 0)) & _$((_temp + 1))) ^ (_$((_temp + 0)) & _$((_temp + 2))) ^ (_$((_temp + 1)) & _$((_temp + 2)))))
    t2=$(((s0 + ma) & ((65535 << 16) | 65535)))
    : $((_$((_temp + 7)) = _$((_temp + 6))))
    : $((_$((_temp + 6)) = _$((_temp + 5))))
    : $((_$((_temp + 5)) = _$((_temp + 4))))
    : $((_$((_temp + 4)) = (_$((_temp + 3)) + t1) & ((65535 << 16) | 65535)))
    : $((_$((_temp + 3)) = _$((_temp + 2))))
    : $((_$((_temp + 2)) = _$((_temp + 1))))
    : $((_$((_temp + 1)) = _$((_temp + 0))))
    : $((_$((_temp + 0)) = (t1 + t2) & ((65535 << 16) | 65535)))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 8 ]; do
    : $((_$((_hash + i)) = (_$((_hash + i)) + _$((_temp + i))) & ((65535 << 16) | 65535)))
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
  printf \\$(((_$((digits + (15 & (byte >> 4)))))/64))$(((_$((digits + (15 & (byte >> 4)))))/8%8))$(((_$((digits + (15 & (byte >> 4)))))%8))
  printf \\$(((_$((digits + (15 & byte))))/64))$(((_$((digits + (15 & byte))))/8%8))$(((_$((digits + (15 & byte))))%8))
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
      : $((_$((_buf + n)) = 128))
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
        : $((_$((_buf + 64 - i)) = 255 & _nbits))
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
  printf \\$(((__NEWLINE__)/64))$(((__NEWLINE__)/8%8))$(((__NEWLINE__)%8))
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

# Character constants
readonly __NEWLINE__=10
# Runtime library

unpack_escaped_string() { # $1 = string, $2 = size (optional)
  __buf="$1"
  # Allocates enough space for all characters, assuming that no character is escaped
  _malloc __addr $((${2:-${#__buf} + 1}))
  __ptr=$__addr
  __end=$((__ptr + ${2:-${#__buf} + 1})) # End of allocated memory
  while [ -n "$__buf" ] ; do
    case "$__buf" in
      '\'*)
        __buf="${__buf#?}" # Remove the current char from $__buf
        case "$__buf" in
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
          *) echo "invalid escape in string: $__buf"; exit 1 ;;
        esac
        __buf="${__buf#?}" # Remove the current char from $__buf
        ;;
      *)
        __c=$(printf "%d" "'${__buf%"${__buf#?}"}"); __c=$((__c > 0 ? __c : 256 + __c))
        __buf="${__buf#?}" # Remove the current char from $__buf
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
unpack_line() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?
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
    __c=$(printf "%d" "'$__char"); __c=$((__c > 0 ? __c : 256 + __c))
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
