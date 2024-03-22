# Memory management

# C constants
# readonly is POSIX compliant: https://pubs.opengroup.org/onlinepubs/009695299/utilities/readonly.html
# Idea: Inlining those constants in the generated code could make it faster, at the cost of readability.
readonly _NULL=0
readonly _EOF=-1

strict_alloc() {
  if [ $__FREE_UNSETS_VARS -eq 1 ]; then
    : $((_$__ALLOC = $1)) # Save allocation size
    : $((__ALLOC += 1))
  fi
  strict_alloc_res=$__ALLOC
  : $((__ALLOC += $1))
  # Need to initialize the memory to 0 or else `set -u` will complain
  if [ $__STRICT_MODE -eq 1 ]; then
    strict_alloc_ix=$strict_alloc_res
    while [ $strict_alloc_ix -lt $__ALLOC ]; do
      : $((_$strict_alloc_ix=0))
      : $((strict_alloc_ix += 1))
    done
  fi
}

make_argv() {
  make_argv_argc=$1; shift;
  strict_alloc $make_argv_argc ; make_argv_ptr=$strict_alloc_res
  make_argv_ix=$make_argv_ptr

  while [ $# -ge 1 ]; do
    unpack_string "$1"
    : $((_$make_argv_ix = $unpack_string_addr))
    : $((make_argv_ix += 1))
    shift
  done
}

push_data() {
  : $((_$__ALLOC=$1))
  : $((__ALLOC += 1))
}

unpack_array() {
  unpack_array_addr=$__ALLOC
  while [ $# -gt 0 ] ; do
    push_data $1
    shift
  done
}

# Push a Shell string to the VM heap. Returns a reference to the string in $addr.
unpack_string() {
  unpack_string_addr=$__ALLOC
  unpack_string_src_buf="$1"
  while [ -n "$unpack_string_src_buf" ] ; do
    unpack_string_char="$unpack_string_src_buf"                      # remember current buffer
    unpack_string_rest="${unpack_string_src_buf#?}"                  # remove the first char
    unpack_string_char="${unpack_string_char%"$unpack_string_rest"}" # remove all but first char
    unpack_string_src_buf="${unpack_string_src_buf#?}"               # remove the current char from $src_buf
    char_to_int "$unpack_string_char"
    push_data $__c
  done
  push_data 0
}

# See https://en.wikipedia.org/wiki/Escape_sequences_in_C#Table_of_escape_sequences
unpack_escaped_string() {
  unpack_string_addr=$__ALLOC
  unpack_string_src_buf="$1"
  while [ -n "$unpack_string_src_buf" ] ; do
    unpack_string_char="$unpack_string_src_buf"                      # remember current buffer
    unpack_string_rest="${unpack_string_src_buf#?}"                  # remove the first char
    unpack_string_char="${unpack_string_char%"$unpack_string_rest"}" # remove all but first char
    unpack_string_src_buf="${unpack_string_src_buf#?}"               # remove the current char from $src_buf
    if [ '\' = "$unpack_string_char" ] ; then
      unpack_string_char="$unpack_string_src_buf"                      # remember current buffer
      unpack_string_rest="${unpack_string_src_buf#?}"                  # remove the first char
      unpack_string_char="${unpack_string_char%"$unpack_string_rest"}" # remove all but first char
      unpack_string_src_buf="${unpack_string_src_buf#?}"               # remove the current char from $src_buf
      case "$unpack_string_char" in
        'a') __c=7 ;;
        'b') __c=8 ;;
        'f') __c=12 ;;
        'n') __c=10 ;;
        'r') __c=13 ;;
        't') __c=9 ;;
        'v') __c=11 ;;
        '\') __c=92 ;;
        '"') __c=34 ;;
        "'") __c=39 ;;
        '?') __c=63 ;;
        '$') __c=36 ;; # Not in C, used to escape variable expansion between double quotes
        *) echo "invalid escape in string: $unpack_string_char"; exit 1 ;;
      esac
      push_data $__c
    else
      char_to_int "$unpack_string_char"
      push_data $__c
    fi
  done
  push_data 0
}

# Convert a VM string reference to a Shell string.
# $res is set to the result, and $len is set to the length of the string.
pack_string() {
  pack_string_addr=$1; shift
  pack_string_max_len=100000000
  pack_string_delim=0
  pack_string_len=0
  pack_string_res=""
  if [ $# -ge 1 ] ; then pack_string_delim=$1   ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then pack_string_max_len=$1 ; shift ; fi # Optional max length
  while [ $((_$pack_string_addr)) -ne $pack_string_delim ] && [ $pack_string_max_len -gt $pack_string_len ] ; do
    pack_string_char=$((_$pack_string_addr))
    pack_string_addr=$((pack_string_addr + 1))
    pack_string_len=$((pack_string_len + 1))
    case $pack_string_char in
      10) pack_string_res="$pack_string_res\n" ;; # 10 == '\n'
      *)  int_to_char "$pack_string_char"; pack_string_res="$pack_string_res$__char" ;;
    esac
  done
}

# Emit a C-string line by line so that whitespace isn't mangled
print_string() {
  print_string_addr=$1; shift
  print_string_max_len=100000000
  print_string_delim=0
  print_string_len=0
  print_string_acc=""
  if [ $# -ge 1 ] ; then print_string_delim=$1   ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then print_string_max_len=$1 ; shift ; fi # Optional max length
  while [ $((_$print_string_addr)) -ne $print_string_delim ] && [ $print_string_max_len -gt $print_string_len ] ; do
    print_string_char=$((_$print_string_addr))
    print_string_addr=$((print_string_addr + 1))
    print_string_len=$((print_string_len + 1))
    case $print_string_char in
      10) # 10 == '\n'
        printf "%s\n" "$print_string_acc"
        print_string_acc="" ;;
      *)
        int_to_char $print_string_char
        print_string_acc="$print_string_acc$__char" ;;
    esac
  done
  printf "%s" "$print_string_acc"
}

char_to_int() {
  case $1 in
    '0') __c=48 ;;
    '1') __c=49 ;;
    '2') __c=50 ;;
    '3') __c=51 ;;
    '4') __c=52 ;;
    '5') __c=53 ;;
    '6') __c=54 ;;
    '7') __c=55 ;;
    '8') __c=56 ;;
    '9') __c=57 ;;
    'a') __c=97 ;;
    'b') __c=98 ;;
    'c') __c=99 ;;
    'd') __c=100 ;;
    'e') __c=101 ;;
    'f') __c=102 ;;
    'g') __c=103 ;;
    'h') __c=104 ;;
    'i') __c=105 ;;
    'j') __c=106 ;;
    'k') __c=107 ;;
    'l') __c=108 ;;
    'm') __c=109 ;;
    'n') __c=110 ;;
    'o') __c=111 ;;
    'p') __c=112 ;;
    'q') __c=113 ;;
    'r') __c=114 ;;
    's') __c=115 ;;
    't') __c=116 ;;
    'u') __c=117 ;;
    'v') __c=118 ;;
    'w') __c=119 ;;
    'x') __c=120 ;;
    'y') __c=121 ;;
    'z') __c=122 ;;
    'A') __c=65 ;;
    'B') __c=66 ;;
    'C') __c=67 ;;
    'D') __c=68 ;;
    'E') __c=69 ;;
    'F') __c=70 ;;
    'G') __c=71 ;;
    'H') __c=72 ;;
    'I') __c=73 ;;
    'J') __c=74 ;;
    'K') __c=75 ;;
    'L') __c=76 ;;
    'M') __c=77 ;;
    'N') __c=78 ;;
    'O') __c=79 ;;
    'P') __c=80 ;;
    'Q') __c=81 ;;
    'R') __c=82 ;;
    'S') __c=83 ;;
    'T') __c=84 ;;
    'U') __c=85 ;;
    'V') __c=86 ;;
    'W') __c=87 ;;
    'X') __c=88 ;;
    'Y') __c=89 ;;
    'Z') __c=90 ;;
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
    48)  __char="0" ;;
    49)  __char="1" ;;
    50)  __char="2" ;;
    51)  __char="3" ;;
    52)  __char="4" ;;
    53)  __char="5" ;;
    54)  __char="6" ;;
    55)  __char="7" ;;
    56)  __char="8" ;;
    57)  __char="9" ;;
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

# Define a string, and return a reference to it in $defstr_return_var.
# If $defstr_return_var is already defined, return the reference to the string in $defstr_return_var.
# Note that it's up to the caller of defstr to ensure that for each variable, there is only one unique string.
defstr() {
  # Check if we are passing a hash
  defstr_return_var=$1
  defstr_str=$2

  set +u # Necessary to allow $defstr_return_var to be empty
  if [ $(($defstr_return_var)) -eq 0 ]; then
    unpack_escaped_string "$defstr_str"
    : $(( $defstr_return_var = unpack_string_addr ))
  fi
  set -u
}

# Primitives

_putchar() { printf \\$(($1/64))$(($1/8%8))$(($1%8)) ; }


__stdin_buf=
_getchar()                                          # get next char from source into $__c
{
  if [ -z "$__stdin_buf" ] ; then                   # need to get next line when buffer empty
    IFS=                                            # don't split input
    if read -r __stdin_buf ; then                   # read next line into $src_buf
      if [ -z "$__stdin_buf" ] ; then               # an empty line implies a newline character
        : $(($1 = 10))                              # next getchar call will read next line
        return
      fi
    else
      : $(($1 = -1))                                # EOF reached when read fails
      return
    fi
  else
    __stdin_buf="${__stdin_buf#?}"                  # remove the current char from $src_buf
    if [ -z "$__stdin_buf" ] ; then                 # end of line if the buffer is now empty
      : $(($1 = 10))
      return
    fi
  fi

  # current character is at the head of $__stdin_buf
  __stdin_char="$__stdin_buf"                    # remember current buffer
  __stdin_rest="${__stdin_buf#?}"                # remove the first __stdin_char
  __stdin_char="${__stdin_char%"$__stdin_rest"}" # remove all but first char
  char_to_int "$__stdin_char"
  : $(($1 = __c))
}

_exit() { echo \"Exiting with code $1\"; exit $1; }

_malloc() { # $2 = malloc_size
  malloc_size=$2
  strict_alloc $malloc_size
  : $(($1 = strict_alloc_res))
}

_free() { # $1 = free_ptr
  if [ $__FREE_UNSETS_VARS -eq 1 ]; then
    free_ptr=$1
    free_size=$((_$((free_ptr - 1)))) # Get size of allocation
    while [ $free_size -gt 0 ]; do
      unset "_$free_ptr"
      : $((free_ptr += 1))
      : $((free_size -= 1))
    done
  fi
}

_printf() { # $1 = printf format string, $2... = printf args
  printf_fmt_ptr=$1; shift
  printf_mod=0
  while [ "$((_$printf_fmt_ptr))" -ne 0 ] ; do
    printf_head=$((_$printf_fmt_ptr))
    int_to_char $printf_head; printf_head_char=$__char
    printf_fmt_ptr=$((printf_fmt_ptr + 1))
    if [ $printf_mod -eq 1 ] ; then
      case $printf_head_char in
        'd') # 100 = 'd' Decimal integer
          printf_imm=$1; shift
          printf "%d" $printf_imm
          ;;
        'c') # 99 = 'c' Character
          printf_char=$1; shift
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          int_to_char $printf_char
          printf "%c" "$__char"
          ;;
        'x') # 120 = 'x' Hexadecimal integer
          printf_imm=$1; shift
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          printf "%x" $printf_imm
          ;;
        's') # 115 = 's' String
          printf_str_ref=$1; shift
          print_string $printf_str_ref
          ;;
        '.') # String with length. %.*s will print the first 4 characters of the string
          pack_string $printf_fmt_ptr 0 2 # Read next 2 characters
          printf_fmt_ptr=$((printf_fmt_ptr + 2))
          if [ "$pack_string_res" = "*s" ]; then
            printf_len=$1; shift
            printf_str_ref=$1; shift
            print_string $printf_str_ref 0 $printf_len
          else
            echo "Unknown format specifier: %.$pack_string_res" ; exit 1
          fi
          ;;
        [0-9])                         # parse integer
          # Get max length (with padding)
          pack_string $printf_fmt_ptr 46 # Read until '.' or end of string
          printf_fmt_ptr=$((printf_fmt_ptr + pack_string_len + 1))
          printf_min_len="$printf_head_char$pack_string_res" # Don't forget the first digit we've already read

          # Get string length
          pack_string $printf_fmt_ptr 115 # Read until 's' or end of string
          printf_fmt_ptr=$((printf_fmt_ptr + pack_string_len))
          printf_str_len=$pack_string_res

          printf_head=$((_$printf_fmt_ptr))
          printf_head_char=$(printf "\\$(printf "%o" "$printf_head")") # Decode
          printf_fmt_ptr=$((printf_fmt_ptr + 1))
          if [ "$printf_head_char" = 's' ]; then
            printf_str_ref=$1; shift
            # Count length of string with pack_string but don't use packed string
            pack_string $printf_str_ref 0 $printf_str_len
            printf_str_padding=""
            : $((printf_padding_len = $printf_min_len - $pack_string_len))
            while [ $printf_padding_len -gt 0 ]; do # Pad string so it has at least $min_len characters
              printf_str_padding=" $printf_str_padding"
              : $((printf_padding_len -= 1))
              done
            printf "%s" "$printf_str_padding" # Pad string
            print_string $printf_str_ref 0 $printf_str_len # Print string
          else
            echo "Unknown format specifier: '%$printf_min_len.$printf_str_len$printf_head_char'" ; exit 1;
          fi
          ;;
        *)
          echo "Unknown format specifier %$printf_head_char"; exit 1
      esac
      printf_mod=0
    else
      case $printf_head in
        10) printf "\n" ;;  # 10 == '\n'
        37) printf_mod=1 ;; # 37 == '%'
        *) printf "$printf_head_char" ;; # Decode
      esac
    fi
  done
}

# We represent file descriptors as strings. That means that modes and offsets do not work.
# These limitations are acceptable since c4.cc does not use them.
# TODO: Packing and unpacking the string is a lazy way of copying a string
_open() { # $2: File name, $3: Mode
  pack_string $2
  unpack_string "$pack_string_res"
  : $(($1 = unpack_string_addr))
}

_read() { # $2: File descriptor, $3: Buffer, $3: Maximum number of bytes to read
  read_fd=$2
  read_buf=$3
  read_count=$4
  pack_string $read_fd
  read_n_char $read_count $read_buf < "$pack_string_res" # We don't want to use cat because it's not pure Shell
  : $(($1 = read_n_char_len))
}

# File descriptor is just a string, nothing to close
_close() { # $2: File descriptor
  : $(($1 = 0))
}

# Used to implement the read instruction.
# Does not work with NUL characters.
read_n_char() {
  read_n_char_count=$1
  read_n_char_buf_ptr=$2
  read_n_char_len=0
  while [ $read_n_char_count -ne 0 ] ; do
    get_char
    case "$get_char_char" in
      EOF) break ;;
      NEWLINE) read_n_char_code=10 ;; # 10 == '\n'
      *) char_to_int "$get_char_char"; read_n_char_code=$__c ;;
    esac

    : $((_$read_n_char_buf_ptr=$read_n_char_code))
    : $((read_n_char_buf_ptr += 1))
    : $((read_n_char_count -= 1))
    : $((read_n_char_len += 1))
  done
}

# Read the file, and return a file descriptor to the file.
# The file descriptor is just a cursor and a string, so closing just frees up the object.
_fopen() { # $2: File name, $3: Mode
  pack_string $2
  fopen_fd=$__ALLOC                                 # Allocate new FD
  : $((_$((fopen_fd)) = 0))                         # Initialize cursor to 0
  fopen_buffer=$((fopen_fd + 1))                    # Buffer starts after cursor
  read_all_char $fopen_buffer < "$pack_string_res"
  : $((_$((fopen_buffer + read_all_char_len))=-1))  # Terminate buffer with EOF character
  __ALLOC=$((__ALLOC + read_all_char_len + 2))      # Update __ALLOC to the new end of the heap
  : $(($1 = fopen_fd))
}

_fclose() { # $2: File descriptor
  _free $2 # Release file descriptor buffer
  : $(($1 = 0))
}

# Only supports item size = 1 for now
_fread() { # $2: Buffer, $3: Item size, $4: Number of items to read, $5: File descriptor
  fread_buf=$2
  fread_item_size=$3
  fread_count=$4
  fread_fd=$5
  fread_len=0
  # TODO: Support all item sizes. One difficulty is that we can't read partial items.
  if [ $fread_item_size -ne 1 ]; then echo "fread: item size must be 1" ; exit 1 ; fi
  fread_fd_buffer=$((fread_fd + 1)) # Buffer starts at fd + 1
  # As long as there are items to read and we haven't reached EOF
  while [ $fread_count -ne 0 ] && [ $((_$fread_fd_buffer)) -ne -1 ] ; do
    : $((_$fread_buf=_$fread_fd_buffer))
    : $((fread_buf += 1))
    : $((fread_fd_buffer += 1))
    : $((fread_count -= 1))
    : $((fread_len += 1))
  done
  # Update cursor
  : $((_$fread_fd = $fread_len))
  : $(($1 = fread_len))
}

_fgetc() { # $2: File descriptor
  fgetc_fd=$2
  fgetc_cursor=$((_$fgetc_fd))
  fgetc_fd_buffer=$((fgetc_fd + 1)) # Buffer starts at fd + 1
  fgetc_char=$((_$((fgetc_fd_buffer + fgetc_cursor))))
  : $((_$fgetc_fd += 1)) # Update cursor
  : $(($1 = fgetc_char))
}

read_all_char() {
  read_all_char_buf_ptr=$1
  read_all_char_len=0
  while : ; do
    get_char
    case "$get_char_char" in
      EOF) break ;;
      NEWLINE) read_all_char_code=10 ;; # 10 == '\n'
      *) char_to_int "$get_char_char"; read_all_char_code=$__c ;;
    esac

    : $((_$read_all_char_buf_ptr=$read_all_char_code))
    : $((read_all_char_buf_ptr += 1))
    : $((read_all_char_len += 1))
  done
}

get_char_src_buf=
get_char()                           # get next char from source into $get_char_char
{
  if [ -z "$get_char_src_buf" ] ; then        # need to get next line when buffer empty
    IFS=                             # don't split input
    if read -r get_char_src_buf ; then        # read next line into $src_buf
      if [ -z "$get_char_src_buf" ] ; then    # an empty line implies a newline character
        get_char_char=NEWLINE                 # next get_char call will read next line
        return
      fi
    else
      get_char_char=EOF                       # EOF reached when read fails
      return
    fi
  else
    get_char_src_buf="${get_char_src_buf#?}"           # remove the current char from $src_buf
    if [ -z "$get_char_src_buf" ] ; then      # end of line if the buffer is now empty
      get_char_char=NEWLINE
      return
    fi
  fi

  # current character is at the head of $src_buf

  get_char_char="$get_char_src_buf"                    # remember current buffer
  get_char_rest="${get_char_src_buf#?}"                # remove the first get_char_char
  get_char_char="${get_char_char%"$get_char_rest"}"             # remove all but first char
}

_memset() { # $2: Pointer, $3: Value, $4: Length
  memset_ptr=$2
  memset_val=$3
  memset_len=$4
  memset_ix=0
  while [ $memset_ix -lt $memset_len ]; do
    : $((_$((memset_ptr + memset_ix)) = memset_val))
    : $((memset_ix += 1))
  done
  : $(($1 = memset_ptr))
}

_memcmp() { # $2: Pointer 1, $3: Pointer 2, $4: Length
  memcmp_op1=$2
  memcmp_op2=$3
  memcmp_len=$4
  memcmp_ix=0
  memcmp_a=0
  while [ $memcmp_ix -lt $memcmp_len ]; do
    if [ $((_$((memcmp_op1 + memcmp_ix)))) -ne $((_$((memcmp_op2 + memcmp_ix)))) ] ; then
      # From man page: returns the difference between the first two differing bytes (treated as unsigned char values
      : $(($1 = _$((memcmp_op1 + memcmp_ix)) - _$((memcmp_op2 + memcmp_ix))))
      return
    fi
    : $((memcmp_ix = memcmp_ix + 1))
  done
  : $(($1 = 0))
}

# Debug

_show_heap() {
  set +u
  show_heap_ix=1
  show_heap_elided=0
  echo "    Heap:"
  while [ $show_heap_ix -lt $__ALLOC ]; do
    show_heap_location=_$show_heap_ix
    # Safe way of checking if the variable is defined or not. With +u, we could also check if it's empty.
    eval "if [ -z \${$show_heap_location+x} ]; then show_heap_undefined=1; else show_heap_undefined=0; fi"
    if [ $show_heap_undefined -eq 1 ]; then
      show_heap_elided=1
    else
      if [ "$show_heap_elided" -eq 1 ]; then
        echo "        ..."
        show_heap_elided=0
      fi

      show_heap_ascii=$((_$show_heap_ix))
      show_heap_char=""
      if [ $show_heap_ascii -ge 31 ] && [ $show_heap_ascii -le 127 ] ; then
        int_to_char $show_heap_ascii
        show_heap_char=$__char
      fi
      echo "        _$show_heap_ix = $show_heap_ascii  ($show_heap_char)"
    fi
    : $((show_heap_ix += 1))
  done
  set -u
}

_show_arg_stack() {
  set +u
  show_arg_stack_ix=1
  echo "    Heap:"
  while [ $show_arg_stack_ix -le $((__SP + 1)) ]; do
    eval "val=\$_data_$show_arg_stack_ix"
    echo "        _$show_arg_stack_ix = $val"
    : $((show_arg_stack_ix += 1))
  done
}

_show_fd() {
  if [ $# -eq 2 ]; then
    shift
  fi
  echo "==== File descriptor ===="
  echo "Address: $1"
  echo "Cursor: $((_$1))"
  # echo "=====    Content    ====="
  # print_string $((_$(($1 + 1))))
}
