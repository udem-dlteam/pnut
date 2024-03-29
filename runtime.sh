# Memory management

# C constants
# readonly is POSIX compliant: https://pubs.opengroup.org/onlinepubs/009695299/utilities/readonly.html
# Idea: Inlining those constants in the generated code could make it faster, at the cost of readability.
readonly _NULL=0
readonly _EOF=-1

__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

# Begins a new object on the heap. The object is uninitialized and its length is $1.
new_object() {
  __OBJ_START=$__ALLOC          # Point to the header of the new object
  __OBJ_LEN=$1                  # Initialize object length, will be used to write the header when finalizing the object.
  if [ $__FREE_UNSETS_VARS -eq 1 ]; then
    __addr=$((__ALLOC + 1))
    : $((__ALLOC += $1 + 1))
  else
    __addr=$__ALLOC
    : $((__ALLOC += $1))
  fi
}

# Add $1 to the end of the object, growing it by 1 byte.
extend_object() {
  : $((_$__ALLOC = $1))
  : $((__ALLOC   += 1))
  : $((__OBJ_LEN += 1))
}

# Finalize the object, writing the header and returning the address of the object.
finalize_object() { # $1 = (optional) number of bytes to add to the object
  if [ $# -eq 1 ]; then
    : $((__ALLOC += $1))
    : $((__OBJ_LEN += $1))
  fi
  if [ $__FREE_UNSETS_VARS -eq 1 ]; then
    : $((_$__OBJ_START = __OBJ_LEN))  # Write header
    : $(( __addr = __OBJ_START + 1))  # Return address of object, after the header
    # unset __OBJ_START
    # unset __OBJ_LEN
  else
    __addr=$__OBJ_START
  fi
}

alloc() {
  # When free isn't a no-op, we need to tag all objects with their size
  if [ $__FREE_UNSETS_VARS -eq 1 ]; then
    : $((_$__ALLOC = $1)) # Save allocation size
    : $((__ALLOC += 1))
  fi
  __addr=$__ALLOC
  : $((__ALLOC += $1))
}

# Initialize the memory to 0
initialize_memory() { # $1 = address, $2 = length
  __ix=$1
  __last=$(($1 + $2))
  while [ $__ix -lt $__last ]; do
    : $((_$__ix=0))
    : $((__ix += 1))
  done
}

make_argv() {
  __argc=$1; shift;
  alloc $__argc # Allocate enough space for all elements. No need to initialize.
  __argv=$__addr
  __ptr=$__addr # Saving value because its overwritten by unpack_string

  while [ $# -ge 1 ]; do
    unpack_string "$1"
    : $((_$__ptr = $__addr))
    : $((__ptr += 1))
    shift
  done
}

unpack_array() {
  alloc $# # Allocate enough space for all elements. No need to initialize.
  __ptr=$__addr
  while [ $# -gt 0 ] ; do
    : $((_$__ptr = $1))
    : $((__ptr += 1))
    shift
  done
}

# Push a Shell string to the VM heap. Returns a reference to the string in $__addr.
unpack_string() {
  new_object 0
  __buf="$1"
  while [ -n "$__buf" ] ; do
    __char="${__buf%"${__buf#?}"}"   # remove all but first char
    __buf="${__buf#?}"               # remove the current char from $__buf
    char_to_int "$__char"
    extend_object $__c
  done
  extend_object 0
  finalize_object
}

# See https://en.wikipedia.org/wiki/Escape_sequences_in_C#Table_of_escape_sequences
# Not matching on the most common characters like in _getchar because this
# function reads strings with a different distribution.
unpack_escaped_string() {
  new_object 0
  __buf="$1"
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
        char_to_int "${__buf%"${__buf#?}"}" # remove all but first char
        __buf="${__buf#?}"               # remove the current char from $__buf
        ;;
    esac
    extend_object $__c
  done
  extend_object 0
  finalize_object
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
  while [ $((_$__addr)) -ne $__delim ] && [ $__max_len -gt $__len ] ; do
    __char=$((_$__addr))
    __addr=$((__addr + 1))
    __len=$((__len + 1))
    case $__char in
      10) __res="$__res\n" ;; # 10 == '\n'
      *)  int_to_char "$__char"; __res="$__res$__char" ;;
    esac
  done
}

# Emit a C-string line by line so that whitespace isn't mangled
print_string() {
  __addr=$1; shift
  __max_len=100000000
  __delim=0
  __len=0
  __acc=""
  if [ $# -ge 1 ] ; then __delim=$1   ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then __max_len=$1 ; shift ; fi # Optional max length
  while [ $((_$__addr)) -ne $__delim ] && [ $__max_len -gt $__len ] ; do
    __char=$((_$__addr))
    __addr=$((__addr + 1))
    __len=$((__len + 1))
    case $__char in
      10) # 10 == '\n'
        printf "%s\n" "$__acc"
        __acc="" ;;
      *)
        int_to_char $__char
        __acc="$__acc$__char" ;;
    esac
  done
  printf "%s" "$__acc"
}

char_to_int() {
  case $1 in
    [0-9]) __c=$((48 + $1)) ;;
    # [a-m])
    #   case $1 in
    #     [a-g])
    #       case $1 in
            'a') __c=97 ;;
            'b') __c=98 ;;
            'c') __c=99 ;;
            'd') __c=100 ;;
            'e') __c=101 ;;
            'f') __c=102 ;;
            'g') __c=103 ;;
          # esac ;;
        'h') __c=104 ;;
        'i') __c=105 ;;
        'j') __c=106 ;;
        'k') __c=107 ;;
        'l') __c=108 ;;
        'm') __c=109 ;;
    #   esac ;;
    # [n-z])
    #   case $1 in
    #     [n-t])
    #       case $1 in
            'n') __c=110 ;;
            'o') __c=111 ;;
            'p') __c=112 ;;
            'q') __c=113 ;;
            'r') __c=114 ;;
            's') __c=115 ;;
            't') __c=116 ;;
          # esac ;;
        'u') __c=117 ;;
        'v') __c=118 ;;
        'w') __c=119 ;;
        'x') __c=120 ;;
        'y') __c=121 ;;
        'z') __c=122 ;;
    #   esac ;;
    # [A-M])
    #   case $1 in
    #     [A-G])
    #       case $1 in
            'A') __c=65 ;;
            'B') __c=66 ;;
            'C') __c=67 ;;
            'D') __c=68 ;;
            'E') __c=69 ;;
            'F') __c=70 ;;
            'G') __c=71 ;;
          # esac ;;
        'H') __c=72 ;;
        'I') __c=73 ;;
        'J') __c=74 ;;
        'K') __c=75 ;;
        'L') __c=76 ;;
        'M') __c=77 ;;
    #   esac ;;
    # [N-Z])
    #   case $1 in
    #     [N-T])
    #       case $1 in
            'N') __c=78 ;;
            'O') __c=79 ;;
            'P') __c=80 ;;
            'Q') __c=81 ;;
            'R') __c=82 ;;
            'S') __c=83 ;;
            'T') __c=84 ;;
          # esac ;;
        'U') __c=85 ;;
        'V') __c=86 ;;
        'W') __c=87 ;;
        'X') __c=88 ;;
        'Y') __c=89 ;;
        'Z') __c=90 ;;
      # esac ;;
    ' ') __c=32 ;;
    '!') __c=33 ;;
    '"') __c=34 ;;
    '#') __c=35 ;;
    '$') __c=36 ;;
    '%') __c=37 ;;
    '&') __c=38 ;;
    "'") __c=39 ;;
    '(') __c=40 ;;
    ')') __c=41 ;;
    '*') __c=42 ;;
    '+') __c=43 ;;
    ',') __c=44 ;;
    '-') __c=45 ;;
    '.') __c=46 ;;
    '/') __c=47 ;;
    ':') __c=58 ;;
    ';') __c=59 ;;
    '<') __c=60 ;;
    '=') __c=61 ;;
    '>') __c=62 ;;
    '?') __c=63 ;;
    '@') __c=64 ;;
    '[') __c=91 ;;
    '\') __c=92 ;;
    ']') __c=93 ;;
    '^') __c=94 ;;
    '_') __c=95 ;;
    '`') __c=96 ;;
    '{') __c=123 ;;
    '|') __c=124 ;;
    '}') __c=125 ;;
    '~') __c=126 ;;
    *)
      echo "Invalid character: $1" ; exit 1
      __c=$(LC_CTYPE=C printf "%d" "'$1") ;;
  esac
}

int_to_char() {
  case $1 in
    [48-57]) __char=$(($1 - 48)) ;;
    97)  __char="a" ;;
    98)  __char="b" ;;
    99)  __char="c" ;;
    100) __char="d" ;;
    101) __char="e" ;;
    102) __char="f" ;;
    103) __char="g" ;;
    104) __char="h" ;;
    105) __char="i" ;;
    106) __char="j" ;;
    107) __char="k" ;;
    108) __char="l" ;;
    109) __char="m" ;;
    110) __char="n" ;;
    111) __char="o" ;;
    112) __char="p" ;;
    113) __char="q" ;;
    114) __char="r" ;;
    115) __char="s" ;;
    116) __char="t" ;;
    117) __char="u" ;;
    118) __char="v" ;;
    119) __char="w" ;;
    120) __char="x" ;;
    121) __char="y" ;;
    122) __char="z" ;;
    65)  __char="A" ;;
    66)  __char="B" ;;
    67)  __char="C" ;;
    68)  __char="D" ;;
    69)  __char="E" ;;
    70)  __char="F" ;;
    71)  __char="G" ;;
    72)  __char="H" ;;
    73)  __char="I" ;;
    74)  __char="J" ;;
    75)  __char="K" ;;
    76)  __char="L" ;;
    77)  __char="M" ;;
    78)  __char="N" ;;
    79)  __char="O" ;;
    80)  __char="P" ;;
    81)  __char="Q" ;;
    82)  __char="R" ;;
    83)  __char="S" ;;
    84)  __char="T" ;;
    85)  __char="U" ;;
    86)  __char="V" ;;
    87)  __char="W" ;;
    88)  __char="X" ;;
    89)  __char="Y" ;;
    90)  __char="Z" ;;
    32)  __char=" " ;;
    33)  __char="!" ;;
    34)  __char="\"" ;;
    35)  __char="#" ;;
    36)  __char="$" ;;
    37)  __char="%" ;;
    38)  __char="&" ;;
    39)  __char="'" ;;
    40)  __char="(" ;;
    41)  __char=")" ;;
    42)  __char="*" ;;
    43)  __char="+" ;;
    44)  __char="," ;;
    45)  __char="-" ;;
    46)  __char="." ;;
    47)  __char="/" ;;
    58)  __char=":" ;;
    59)  __char=";" ;;
    60)  __char="<" ;;
    61)  __char="=" ;;
    62)  __char=">" ;;
    63)  __char="?" ;;
    64)  __char="@" ;;
    91)  __char="[" ;;
    92)  __char="\\" ;;
    93)  __char="]" ;;
    94)  __char="^" ;;
    95)  __char="_" ;;
    96)  __char="\`" ;;
    123) __char="{" ;;
    124) __char="|" ;;
    125) __char="}" ;;
    126) __char="~" ;;
    *)   __char=$(printf "\\$(printf "%o" "$1")") ;;
  esac
}

# Define a string, and return a reference to it in the varible taken as argument.
# If the variable is already defined, this function does nothing.
# Note that it's up to the caller to ensure that no 2 strings share the same variable.
defstr() { # $1 = variable name, $2 = string
  set +u # Necessary to allow the variable to be empty
  if [ $(($1)) -eq 0 ]; then
    unpack_escaped_string "$2"
    : $(( $1 = __addr ))
  fi
  set -u
}

# Primitives

_putchar() { printf \\$(($1/64))$(($1/8%8))$(($1%8)) ; }

__stdin_buf=
_getchar()
{
  if [ -z "$__stdin_buf" ] ; then                   # need to get next line when buffer empty
    IFS=                                            # don't split input
    if read -r __stdin_buf ; then                   # read next line into $__stdin_buf
      if [ -z "$__stdin_buf" ] ; then               # an empty line implies a newline character
        : $(($1 = 10))                              # next getchar call will read next line
        return
      fi
    else
      : $(($1 = -1))                                # EOF reached when read fails
      return
    fi
  else
    __stdin_buf="${__stdin_buf#?}"                  # remove the current char from $__stdin_buf
    if [ -z "$__stdin_buf" ] ; then                 # end of line if the buffer is now empty
      : $(($1 = 10))
      return
    fi
  fi

  # The current character is at the head of $__stdin_buf. It will be removed in the next call to getchar.
  # The following cases are ordered by frequency in the C source code and correspond to the letters with more than 1000
  # occurrences See analyze-big-c.py to see the frequency of each character in big.c.
  # Note that adding cases here speeds up all shells except ksh, so the set of optimized characters should be kept small.
  case "$__stdin_buf" in
    " "*) : $(($1 = 32))  ;;
    "e"*) : $(($1 = 101)) ;;
    "="*) : $(($1 = 61))  ;;
    "t"*) : $(($1 = 116)) ;;
    ";"*) : $(($1 = 59))  ;;
    "i"*) : $(($1 = 105)) ;;
    ")"*) : $(($1 = 41))  ;;
    "("*) : $(($1 = 40))  ;;
    "n"*) : $(($1 = 110)) ;;
    "s"*) : $(($1 = 115)) ;;
    "l"*) : $(($1 = 108)) ;;
    "+"*) : $(($1 = 43))  ;;
    "p"*) : $(($1 = 112)) ;;
    "a"*) : $(($1 = 97))  ;;
    "r"*) : $(($1 = 114)) ;;
    "f"*) : $(($1 = 102)) ;;
    "d"*) : $(($1 = 100)) ;;
    "*"*) : $(($1 = 42))  ;;
    *)
      # For some reason, inlining char_to_int_ordered here makes bash significantly slower (2.9s to 3.7s)
      char_to_int "${__stdin_buf%"${__stdin_buf#?}"}" # get the first character
      : $(($1 = __c))
      ;;
  esac
}

_exit() { echo \"Exiting with code $1\"; exit $1; }

_malloc() { # $2 = malloc_size
  alloc $2
  : $(($1 = __addr))
}

# Similar to malloc, but always initializes memory to 0.
_calloc() { # $2 = nitems, $3 = size
  alloc $(($2 * $3))
  initialize_memory $__addr $(($2 * $3))
  : $(($1 = __addr))
}

_free() { # $1 = pointer to object to free
  if [ $__FREE_UNSETS_VARS -eq 1 ]; then
    __ptr=$1
    __size=$((_$((__ptr - 1)))) # Get size of allocation
    while [ $__size -gt 0 ]; do
      unset "_$__ptr"
      : $((__ptr += 1))
      : $((__size -= 1))
    done
  fi
}

_printf() { # $1 = printf format string, $2... = printf args
  __fmt_ptr=$1; shift
  __mod=0
  while [ "$((_$__fmt_ptr))" -ne 0 ] ; do
    __head=$((_$__fmt_ptr))
    int_to_char $__head; __head_char=$__char
    __fmt_ptr=$((__fmt_ptr + 1))
    if [ $__mod -eq 1 ] ; then
      case $__head_char in
        'd') # 100 = 'd' Decimal integer
          printf "%d" $1
          shift
          ;;
        'c') # 99 = 'c' Character
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          int_to_char $1
          printf "%c" "$__char"
          shift
          ;;
        'x') # 120 = 'x' Hexadecimal integer
          printf "%x" $1
          shift
          ;;
        's') # 115 = 's' String
          print_string $1
          shift
          ;;
        '.') # String with length. %.*s will print the first 4 characters of the string
          pack_string $__fmt_ptr 0 2 # Read next 2 characters
          __fmt_ptr=$((__fmt_ptr + 2))
          if [ "$__res" = "*s" ]; then
            print_string $2 0 $1
            shift 2
          else
            echo "Unknown format specifier: %.$__res" ; exit 1
          fi
          ;;
        [0-9])                         # parse integer
          # Get max length (with padding)
          pack_string $__fmt_ptr 46 # Read until '.' or end of string
          __fmt_ptr=$((__fmt_ptr + __len + 1))
          __min_len="$__head_char$__res" # Don't forget the first digit we've already read

          # Get string length
          pack_string $__fmt_ptr 115 # Read until 's' or end of string
          __fmt_ptr=$((__fmt_ptr + __len))
          __str_len=$__res

          __head=$((_$__fmt_ptr))
          __head_char=$(printf "\\$(printf "%o" "$__head")") # Decode
          __fmt_ptr=$((__fmt_ptr + 1))
          if [ "$__head_char" = 's' ]; then
            __str_ref=$1; shift
            # Count length of string with pack_string but don't use packed string
            pack_string $__str_ref 0 $__str_len
            __pad=""
            __padlen=$((__min_len - __len)) # Pad string so it has at least $__min_len characters
            while [ $__padlen -gt 0 ]; do
              __pad=" $__pad"
              : $((__padlen -= 1))
              done
            printf "%s" "$__pad" # Pad string
            print_string $__str_ref 0 $__str_len # Print string
          else
            echo "Unknown format specifier: '%$__min_len.$__str_len$__head_char'" ; exit 1;
          fi
          ;;
        *)
          echo "Unknown format specifier %$__head_char"; exit 1
      esac
      __mod=0
    else
      case $__head in
        10) printf "\n" ;;  # 10 == '\n'
        37) __mod=1 ;; # 37 == '%'
        *) printf "$__head_char" ;; # Decode
      esac
    fi
  done
}

# We represent file descriptors as strings. That means that modes and offsets do not work.
# These limitations are acceptable since c4.cc does not use them.
# TODO: Packing and unpacking the string is a lazy way of copying a string
_open() { # $2: File name, $3: Mode
  pack_string $2
  unpack_string "$__res"
  : $(($1 = __addr))
}

_read() { # $2: File descriptor, $3: Buffer, $3: Maximum number of bytes to read
  __fd=$2
  __buf=$3
  __count=$4
  pack_string $__fd
  read_n_char $__count $__buf < "$__res" # We don't want to use cat because it's not pure Shell
  : $(($1 = __len))
}

# File descriptor is just a string, nothing to close
_close() { # $2: File descriptor
  : $(($1 = 0))
}

# Used to implement the read instruction.
# Does not work with NUL characters.
read_n_char() {
  __count=$1
  __buf_ptr=$2
  __len=0
  while [ $__count -ne 0 ] ; do
    get_char
    if [ $__c -eq -1 ]; then break; fi
    : $((_$__buf_ptr = __c))
    : $((__buf_ptr += 1))
    : $((__count -= 1))
    : $((__len += 1))
  done
}

# Read the file, and return a file descriptor to the file.
# The file descriptor is just a cursor and a string, so closing just frees up the object.
_fopen() { # $2: File name, $3: Mode
  pack_string $2
  new_object 0
  __fd=$__addr                                  # Allocate new FD
  : $((_$((__fd)) = 0))                         # Initialize cursor to 0
  __buf=$((__fd + 1))                           # Buffer starts after cursor
  read_all_char $__buf < "$__res"
  : $((_$((__buf + __len))=-1))                 # Terminate buffer with EOF character
  : $(($1 = __fd))
  finalize_object $((__len + 2))                # 2 is for the cursor and EOF
}

_fclose() { # $2: File descriptor
  _free $2 # Release file descriptor buffer
  : $(($1 = 0))
}

# Only supports item size = 1 for now
_fread() { # $2: Buffer, $3: Item size, $4: Number of items to read, $5: File descriptor
  __buf=$2
  # fread_item_size=$3
  __count=$4
  __fd=$5
  __len=0
  __fdbuf=$((__fd + 1)) # Buffer starts at fd + 1
  # TODO: Support all item sizes. One difficulty is that we can't read partial items.
  if [ $3 -ne 1 ]; then echo "fread: item size must be 1" ; exit 1 ; fi
  # As long as there are items to read and we haven't reached EOF
  while [ $__count -ne 0 ] && [ $((_$__fdbuf)) -ne -1 ] ; do
    : $((_$__buf=_$__fdbuf))
    : $((__buf += 1))
    : $((__fdbuf += 1))
    : $((__count -= 1))
    : $((__len += 1))
  done
  # Update cursor
  : $((_$__fd = $__len))
  : $(($1 = __len))
}

_fgetc() { # $2: File descriptor
  __fd=$2
  __cur=$((_$__fd))
  __buf=$((__fd + 1)) # Buffer starts at fd + 1
  : $((_$__fd += 1)) # Update cursor
  : $(($1 = _$((__buf + __cur))))
}

read_all_char() {
  __ptr=$1
  __len=0
  while : ; do
    get_char
    if [ $__c -eq -1 ]; then break; fi
    : $((_$__ptr = __c))
    : $((__ptr += 1))
    : $((__len += 1))
  done
}

__io_buf=
get_char()                           # get next char from source into $__c
{
  if [ -z "$__io_buf" ] ; then        # need to get next line when buffer empty
    IFS=                              # don't split input
    if read -r __io_buf ; then        # read next line into $__io_buf
      if [ -z "$__io_buf" ] ; then    # an empty line implies a newline character
        __c=10                        # next get_char call will read next line
        return
      fi
    else
      __c=-1                          # EOF reached when read fails
      return
    fi
  else
    __io_buf="${__io_buf#?}"          # remove the current char from $__io_buf
    if [ -z "$__io_buf" ] ; then      # end of line if the buffer is now empty
      __c=10
      return
    fi
  fi

  # current character is at the head of $__io_buf. It will be removed in the next call to getchar.
  char_to_int "${__io_buf%"${__io_buf#?}"}" # remove all but first char
}

_memset() { # $2: Pointer, $3: Value, $4: Length
  __ptr=$2
  __val=$3
  __len=$4
  __ix=0
  while [ $__ix -lt $__len ]; do
    : $((_$((__ptr + __ix)) = __val))
    : $((__ix += 1))
  done
  : $(($1 = __ptr))
}

_memcmp() { # $2: Pointer 1, $3: Pointer 2, $4: Length
  __op1=$2
  __op2=$3
  __len=$4
  __ix=0
  while [ $__ix -lt $__len ]; do
    if [ $((_$((__op1 + __ix)))) -ne $((_$((__op2 + __ix)))) ] ; then
      # From man page: returns the difference between the first two differing bytes (treated as unsigned char values
      : $(($1 = _$((__op1 + __ix)) - _$((__op2 + __ix))))
      return
    fi
    : $((__ix = __ix + 1))
  done
  : $(($1 = 0))
}

# Debug

_show_heap() {
  set +u
  __ix=1
  __elided=0
  echo "    Heap:"
  while [ $__ix -lt $__ALLOC ]; do
    __loc=_$__ix
    # Safe way of checking if the variable is defined or not. With +u, we could also check if it's empty.
    eval "if [ -z \${$__loc+x} ]; then __undef=1; else __undef=0; fi"
    if [ $__undef -eq 1 ]; then
      __elided=1
    else
      if [ "$__elided" -eq 1 ]; then
        echo "        ..."
        __elided=0
      fi

      __ascii=$((_$__ix))
      __char=""
      if [ $__ascii -ge 31 ] && [ $__ascii -le 127 ] ; then
        int_to_char $__ascii
      fi
      echo "        _$__ix = $__ascii  ($__char)"
    fi
    : $((__ix += 1))
  done
  set -u
}

_show_arg_stack() {
  set +u
  __ix=1
  echo "    Local variables stack:"
  while [ $__ix -le $((__SP)) ]; do
    eval "__val=\$save_loc_var_$__ix"
    echo "        _$__ix = $__val"
    : $((__ix += 1))
  done
}

_show_fd() {
  echo "==== File descriptor ===="
  echo "Address: $1"
  echo "Cursor: $((_$1))"
  # echo "=====    Content    ====="
  # print_string $((_$(($1 + 1))))
}
