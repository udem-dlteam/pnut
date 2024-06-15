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
      __c=$(LC_CTYPE=C printf "%d" "'$1")
  esac
}

int_to_char() {
  case $1 in
    48|49|50|51|52|53|54|55|56|57) __char=$(($1 - 48)) ;;
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
    10)  __char="
" ;;
    *)
      echo "Invalid character code: $1" ; exit 1
      __char=$(printf "\\$(printf "%o" "$1")") ;;
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

_putchar() {
  : $(($1 = 0)); shift # Return 0
  printf \\$(($1/64))$(($1/8%8))$(($1%8))
}

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

_exit() {
  : $(($1 = 0)); shift # Return 0
  echo \"Exiting with code $1\"
  exit $1
}

_malloc() { # $2 = malloc_size
  alloc $2
  : $(($1 = __addr))
}

_free() { # $1 = pointer to object to free
  : $(($1 = 0)); shift # Return 0
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
  : $(($1 = 0)); shift # Return 0
  __fmt_ptr=$1; shift
  __mod=0
  while [ "$((_$__fmt_ptr))" -ne 0 ] ; do
    __head=$((_$__fmt_ptr))
    __fmt_ptr=$((__fmt_ptr + 1))
    if [ $__mod -eq 1 ] ; then
      int_to_char $__head; __head_char=$__char
      case $__head_char in
        'd') # 100 = 'd' Decimal integer
          printf "%d" $1
          shift
          ;;
        'c') # 99 = 'c' Character
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          printf \\$(($1/64))$(($1/8%8))$(($1%8))
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
          int_to_char $__head; __head_char=$__char
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
        *) printf \\$(($__head/64))$(($__head/8%8))$(($__head%8)) ;;
      esac
    fi
  done
}

__fopen_fd3=0
__fopen_fd4=0
__fopen_fd5=0
__fopen_fd6=0
__fopen_fd7=0
__fopen_fd8=0
__fopen_fd9=0

next_fd() {
  __i=3
  while [ $__i -lt 10 ]; do
    if [ $((__fopen_fd$__i)) -eq 0 ]; then
      __fd=$__i
      return
    fi
    : $((__i += 1))
  done
  # Some shells don't support fd > 9
  echo "No more file descriptors available" ; exit 1
}

# exec $fd<"file" does not work as expected, and we don't want to use eval
open_fd() { # $1: fd id, $2: file to open
  : $((__fopen_fd$1 = 1)) # Mark the fd as opened
  case $1 in
    1) exec 1< $2 ;;
    2) exec 2< $2 ;;
    3) exec 3< $2 ;;
    4) exec 4< $2 ;;
    5) exec 5< $2 ;;
    6) exec 6< $2 ;;
    7) exec 7< $2 ;;
    8) exec 8< $2 ;;
    9) exec 9< $2 ;;
    *) echo "Unknown fd: $1"; exit 1 ;;
  esac
}

# exec $fd<&- does not work as expected, and we don't want to use eval
close_fd() { # $1: fd id
: $((__fopen_fd$1 = 0)) # Mark the fd as closed
  case $1 in
    1) exec 1<&- ;;
    2) exec 2<&- ;;
    3) exec 3<&- ;;
    4) exec 4<&- ;;
    5) exec 5<&- ;;
    6) exec 6<&- ;;
    7) exec 7<&- ;;
    8) exec 8<&- ;;
    9) exec 9<&- ;;
    *) echo "Unknown fd: $1"; exit 1 ;;
  esac
}

# Read the file, and return a file descriptor to the file.
# The file descriptor fields:
# - 0: Buffer
# - 1: Read cursor
# - 2: Buffer size
# - 3: File descriptor number
# - 4: EOF?
# Because the file must be read line-by-line, and string values can't be
# assigned to dynamic variables, each line is read and then unpacked in the
# buffer.
_fopen() { # $2: File name, $3: Mode
  pack_string $2
  next_fd                       # Get available fd
  open_fd $__fd $__res
  alloc 4                       # Allocate file descriptor object
  : $(( $1 = __addr ))
  alloc 1000                    # Allocate buffer
  : $(( _$((__addr)) = 0 ))     # Initialize buf to ""
  : $((_$(($1 + 0)) = __addr))  # Save buffer address
  : $((_$(($1 + 1)) = 0))       # Initialize cursor to 0
  : $((_$(($1 + 2)) = 200))     # Initial buffer size is 1000
  : $((_$(($1 + 3)) = __fd))    # Save fd id
}

_fclose() { # $2: File descriptor
  __fd_id=$((_$(($2 + 3)) ))    # Fd id is at offset 3
  __buf=$((_$(($2 + 0)) ))      # Buffer starts at offset 1
  _free __ $__buf               # Release file descriptor buffer
  _free __ $2                   # Release file descriptor object
  close_fd $__fd_id
  : $(($1 = 0))
}

# Unpack a Shell string into an appropriately sized buffer
unpack_line() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?
  __fgetc_buf=$1
  __buf=$2
  __ends_with_eof=$3
  while [ ! -z "$__fgetc_buf" ]; do
    case "$__fgetc_buf" in
      " "*) : $(( _$__buf = 32 ))  ;;
      "e"*) : $(( _$__buf = 101 )) ;;
      "="*) : $(( _$__buf = 61 ))  ;;
      "t"*) : $(( _$__buf = 116 )) ;;
      ";"*) : $(( _$__buf = 59 ))  ;;
      "i"*) : $(( _$__buf = 105 )) ;;
      ")"*) : $(( _$__buf = 41 ))  ;;
      "("*) : $(( _$__buf = 40 ))  ;;
      "n"*) : $(( _$__buf = 110 )) ;;
      "s"*) : $(( _$__buf = 115 )) ;;
      "l"*) : $(( _$__buf = 108 )) ;;
      "+"*) : $(( _$__buf = 43 ))  ;;
      "p"*) : $(( _$__buf = 112 )) ;;
      "a"*) : $(( _$__buf = 97 ))  ;;
      "r"*) : $(( _$__buf = 114 )) ;;
      "f"*) : $(( _$__buf = 102 )) ;;
      "d"*) : $(( _$__buf = 100 )) ;;
      "*"*) : $(( _$__buf = 42 ))  ;;
      *)
        char_to_int "${__fgetc_buf%"${__fgetc_buf#?}"}" # get the first character
         : $(( _$__buf = __c ))
        ;;
    esac

    __fgetc_buf=${__fgetc_buf#?}      # Remove the first character
    : $((__buf += 1))                 # Move to the next buffer position
  done

  if [ $__ends_with_eof -eq 0 ]; then # Ends with newline and not EOF?
    : $(( _$__buf = 10))              # Line end with newline
    : $((__buf += 1))
  fi
  : $(( _$__buf = 0))                 # Then \0
}

refill_buffer() { # $1: File descriptor
  __fd=$1
  __buf=$((_$((__fd + 0))))
  __fd_id=$((_$((__fd + 3))))

  IFS=
  if read -r __fgetc_buf <&$__fd_id ; then  # read next line into $__fgetc_buf
    __ends_with_eof=0
  else
    __ends_with_eof=1
  fi

  # Check that the buffer is large enough to unpack the line
  __buf_size=$((_$((__fd + 2)) - 2)) # Minus 2 to account for newline and \0
  __len=${#__fgetc_buf}
  if [ $__len -gt $__buf_size ]; then
    # Free buffer and reallocate a new one double the line size
    __buf_size=$((__len * 2))
    _free __ $__buf
    alloc $__buf_size
    : $((_$((__fd + 0)) = __addr))
    : $((_$((__fd + 2)) = __buf_size))
    __buf=$__addr
  fi

  unpack_line "$__fgetc_buf" $__buf $__ends_with_eof
}

_fgetc() { # $2: File descriptor
  __fd=$2
  __buf=$((_$((__fd + 0))))
  __cur=$((_$((__fd + 1))))

  # The cursor is at the end of the buffer, we need to read the next line
  if [ $((_$((__buf + __cur)))) -eq 0 ]; then
    # Buffer has been read completely, read next line
    refill_buffer $__fd

    __cur=0 # Reset cursor and reload fd fields
    __buf=$((_$((__fd + 0)))) # Reload buffer in case it was reallocated
    if [ $((_$((__buf + __cur)))) -eq 0 ]; then
      : $(($1 = -1)) # EOF
      return
    fi
  fi
  : $(($1 = _$((__buf + __cur))))
  : $((_$((__fd + 1)) = __cur + 1))      # Increment cursor
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
