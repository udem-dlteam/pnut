# Memory management

_NULL=0 # Null pointer, should not be modified. TODO: Make global-var replace NULL with 0?

ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

strict_alloc() {
  strict_alloc_res=$ALLOC
  : $((ALLOC += $1))
  # Need to initialize the memory to 0 or else `set -u` will complain
  if [ $STRICT_MODE -eq 1 ]; then
    strict_alloc_ix=$strict_alloc_res
    while [ $strict_alloc_ix -lt $ALLOC ]; do
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
  : $((_$ALLOC=$1))
  : $((ALLOC += 1))
}

unpack_array() {
  unpack_array_addr=$ALLOC
  while [ $# -gt 0 ] ; do
    push_data $1
    shift
  done
}

# Push a Shell string to the VM heap. Returns a reference to the string in $addr.
unpack_string() {
  unpack_string_addr=$ALLOC
  unpack_string_src_buf="$1"
  while [ -n "$unpack_string_src_buf" ] ; do
    unpack_string_char="$unpack_string_src_buf"                      # remember current buffer
    unpack_string_rest="${unpack_string_src_buf#?}"                  # remove the first char
    unpack_string_char="${unpack_string_char%"$unpack_string_rest"}" # remove all but first char
    unpack_string_src_buf="${unpack_string_src_buf#?}"               # remove the current char from $src_buf
    char_to_int "$unpack_string_char"
    push_data "$char_to_int_code"
  done
  push_data 0
}

# Convert a VM string reference to a Shell string.
# $res is set to the result, and $len is set to the length of the string.
pack_string() {
  pack_string_addr="$1";  shift
  pack_string_max_len=100000000
  pack_string_delim=0
  pack_string_len=0
  pack_string_res=""
  if [ $# -ge 1 ] ; then pack_string_delim="$1" ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then pack_string_max_len="$1" ; shift ; fi # Optional max length
  while [ "$((_$pack_string_addr))" -ne $pack_string_delim ] && [ $pack_string_max_len -gt $pack_string_len ] ; do
    pack_string_char="$((_$pack_string_addr))"
    pack_string_addr=$((pack_string_addr + 1))
    pack_string_len=$((pack_string_len + 1))
    case $pack_string_char in
      10) pack_string_res="$pack_string_res\n" ;; # 10 == '\n'
      *)  int_to_char "$pack_string_char"; pack_string_res="$pack_string_res$int_to_char_char" ;;
    esac
  done
}

# Emit a C-string line by line so that whitespace isn't mangled
print_string() {
  print_string_addr="$1";  shift
  print_string_max_len=100000000
  print_string_delim=0
  print_string_len=0
  print_string_acc=""
  if [ $# -ge 1 ] ; then print_string_delim="$1" ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then print_string_max_len="$1" ; shift ; fi # Optional max length
  while [ "$((_$print_string_addr))" -ne $print_string_delim ] && [ $print_string_max_len -gt $print_string_len ] ; do
    print_string_char="$((_$print_string_addr))"
    print_string_addr=$((print_string_addr + 1))
    print_string_len=$((print_string_len + 1))
    case $print_string_char in
      10) # 10 == '\n'
        printf "%s\n" "$print_string_acc"
        print_string_acc="" ;;
      *)
        int_to_char "$print_string_char"
        print_string_acc="$print_string_acc$int_to_char_char" ;;
    esac
  done
  printf "%s" "$print_string_acc"
}

char_to_int() {
  case $1 in
    "0") char_to_int_code=48 ;;
    "1") char_to_int_code=49 ;;
    "2") char_to_int_code=50 ;;
    "3") char_to_int_code=51 ;;
    "4") char_to_int_code=52 ;;
    "5") char_to_int_code=53 ;;
    "6") char_to_int_code=54 ;;
    "7") char_to_int_code=55 ;;
    "8") char_to_int_code=56 ;;
    "9") char_to_int_code=57 ;;
    "a") char_to_int_code=97 ;;
    "b") char_to_int_code=98 ;;
    "c") char_to_int_code=99 ;;
    "d") char_to_int_code=100 ;;
    "e") char_to_int_code=101 ;;
    "f") char_to_int_code=102 ;;
    "g") char_to_int_code=103 ;;
    "h") char_to_int_code=104 ;;
    "i") char_to_int_code=105 ;;
    "j") char_to_int_code=106 ;;
    "k") char_to_int_code=107 ;;
    "l") char_to_int_code=108 ;;
    "m") char_to_int_code=109 ;;
    "n") char_to_int_code=110 ;;
    "o") char_to_int_code=111 ;;
    "p") char_to_int_code=112 ;;
    "q") char_to_int_code=113 ;;
    "r") char_to_int_code=114 ;;
    "s") char_to_int_code=115 ;;
    "t") char_to_int_code=116 ;;
    "u") char_to_int_code=117 ;;
    "v") char_to_int_code=118 ;;
    "w") char_to_int_code=119 ;;
    "x") char_to_int_code=120 ;;
    "y") char_to_int_code=121 ;;
    "z") char_to_int_code=122 ;;
    "A") char_to_int_code=65 ;;
    "B") char_to_int_code=66 ;;
    "C") char_to_int_code=67 ;;
    "D") char_to_int_code=68 ;;
    "E") char_to_int_code=69 ;;
    "F") char_to_int_code=70 ;;
    "G") char_to_int_code=71 ;;
    "H") char_to_int_code=72 ;;
    "I") char_to_int_code=73 ;;
    "J") char_to_int_code=74 ;;
    "K") char_to_int_code=75 ;;
    "L") char_to_int_code=76 ;;
    "M") char_to_int_code=77 ;;
    "N") char_to_int_code=78 ;;
    "O") char_to_int_code=79 ;;
    "P") char_to_int_code=80 ;;
    "Q") char_to_int_code=81 ;;
    "R") char_to_int_code=82 ;;
    "S") char_to_int_code=83 ;;
    "T") char_to_int_code=84 ;;
    "U") char_to_int_code=85 ;;
    "V") char_to_int_code=86 ;;
    "W") char_to_int_code=87 ;;
    "X") char_to_int_code=88 ;;
    "Y") char_to_int_code=89 ;;
    "Z") char_to_int_code=90 ;;
    " ") char_to_int_code=32 ;;
    "!") char_to_int_code=33 ;;
    "\"") char_to_int_code=34 ;;
    "#") char_to_int_code=35 ;;
    "$") char_to_int_code=36 ;;
    "%") char_to_int_code=37 ;;
    "\&") char_to_int_code=38 ;;
    "'") char_to_int_code=39 ;;
    "(") char_to_int_code=40 ;;
    ")") char_to_int_code=41 ;;
    "\*") char_to_int_code=42 ;;
    "+") char_to_int_code=43 ;;
    ",") char_to_int_code=44 ;;
    "-") char_to_int_code=45 ;;
    ".") char_to_int_code=46 ;;
    "/") char_to_int_code=47 ;;
    ":") char_to_int_code=58 ;;
    ";") char_to_int_code=59 ;;
    "<") char_to_int_code=60 ;;
    "=") char_to_int_code=61 ;;
    ">") char_to_int_code=62 ;;
    "?") char_to_int_code=63 ;;
    "@") char_to_int_code=64 ;;
    "[") char_to_int_code=91 ;;
    "\\") char_to_int_code=92 ;;
    "]") char_to_int_code=93 ;;
    "^") char_to_int_code=94 ;;
    "_") char_to_int_code=95 ;;
    "\`") char_to_int_code=96 ;;
    "{") char_to_int_code=123 ;;
    "|") char_to_int_code=124 ;;
    "}") char_to_int_code=125 ;;
    "~") char_to_int_code=126 ;;
    *)   char_to_int_code=$(LC_CTYPE=C printf "%d" "'$1") ;;
  esac
}

int_to_char() {
  case $1 in
    48)  int_to_char_char="0" ;;
    49)  int_to_char_char="1" ;;
    50)  int_to_char_char="2" ;;
    51)  int_to_char_char="3" ;;
    52)  int_to_char_char="4" ;;
    53)  int_to_char_char="5" ;;
    54)  int_to_char_char="6" ;;
    55)  int_to_char_char="7" ;;
    56)  int_to_char_char="8" ;;
    57)  int_to_char_char="9" ;;
    97)  int_to_char_char="a" ;;
    98)  int_to_char_char="b" ;;
    99)  int_to_char_char="c" ;;
    100) int_to_char_char="d" ;;
    101) int_to_char_char="e" ;;
    102) int_to_char_char="f" ;;
    103) int_to_char_char="g" ;;
    104) int_to_char_char="h" ;;
    105) int_to_char_char="i" ;;
    106) int_to_char_char="j" ;;
    107) int_to_char_char="k" ;;
    108) int_to_char_char="l" ;;
    109) int_to_char_char="m" ;;
    110) int_to_char_char="n" ;;
    111) int_to_char_char="o" ;;
    112) int_to_char_char="p" ;;
    113) int_to_char_char="q" ;;
    114) int_to_char_char="r" ;;
    115) int_to_char_char="s" ;;
    116) int_to_char_char="t" ;;
    117) int_to_char_char="u" ;;
    118) int_to_char_char="v" ;;
    119) int_to_char_char="w" ;;
    120) int_to_char_char="x" ;;
    121) int_to_char_char="y" ;;
    122) int_to_char_char="z" ;;
    65)  int_to_char_char="A" ;;
    66)  int_to_char_char="B" ;;
    67)  int_to_char_char="C" ;;
    68)  int_to_char_char="D" ;;
    69)  int_to_char_char="E" ;;
    70)  int_to_char_char="F" ;;
    71)  int_to_char_char="G" ;;
    72)  int_to_char_char="H" ;;
    73)  int_to_char_char="I" ;;
    74)  int_to_char_char="J" ;;
    75)  int_to_char_char="K" ;;
    76)  int_to_char_char="L" ;;
    77)  int_to_char_char="M" ;;
    78)  int_to_char_char="N" ;;
    79)  int_to_char_char="O" ;;
    80)  int_to_char_char="P" ;;
    81)  int_to_char_char="Q" ;;
    82)  int_to_char_char="R" ;;
    83)  int_to_char_char="S" ;;
    84)  int_to_char_char="T" ;;
    85)  int_to_char_char="U" ;;
    86)  int_to_char_char="V" ;;
    87)  int_to_char_char="W" ;;
    88)  int_to_char_char="X" ;;
    89)  int_to_char_char="Y" ;;
    90)  int_to_char_char="Z" ;;
    32)  int_to_char_char=" " ;;
    33)  int_to_char_char="!" ;;
    34)  int_to_char_char="\"" ;;
    35)  int_to_char_char="#" ;;
    36)  int_to_char_char="$" ;;
    37)  int_to_char_char="%" ;;
    38)  int_to_char_char="&" ;;
    39)  int_to_char_char="'" ;;
    40)  int_to_char_char="(" ;;
    41)  int_to_char_char=")" ;;
    42)  int_to_char_char="*" ;;
    43)  int_to_char_char="+" ;;
    44)  int_to_char_char="," ;;
    45)  int_to_char_char="-" ;;
    46)  int_to_char_char="." ;;
    47)  int_to_char_char="/" ;;
    58)  int_to_char_char=":" ;;
    59)  int_to_char_char=";" ;;
    60)  int_to_char_char="<" ;;
    61)  int_to_char_char="=" ;;
    62)  int_to_char_char=">" ;;
    63)  int_to_char_char="?" ;;
    64)  int_to_char_char="@" ;;
    91)  int_to_char_char="[" ;;
    92)  int_to_char_char="\\" ;;
    93)  int_to_char_char="]" ;;
    94)  int_to_char_char="^" ;;
    95)  int_to_char_char="_" ;;
    96)  int_to_char_char="\`" ;;
    123) int_to_char_char="{" ;;
    124) int_to_char_char="|" ;;
    125) int_to_char_char="}" ;;
    126) int_to_char_char="~" ;;
    *)   int_to_char_char=$(printf "\\$(printf "%o" "$1")") ;;
  esac
}

# This may be a bit slow if used in a loop.
# This function can be called in 2 ways:
# $1 = variable to assign address of string, $2 = string
# $1 = variable to assign address of string, $2 = hash, $3 = string
# Passing the hash makes it faster, but introduces "magic" numbers in the generated code.
init_string() {
  # Check if we are passing a hash
  if [ $# -eq 2 ]; then
    djb2 "$2"
    init_string_return_var=$1
    init_string_hash=$djb2_hash
    init_string_str=$2
  else
    init_string_return_var=$1
    init_string_hash=$2
    init_string_str=$3
  fi

  # FIXME: Replace eval call with check != 0 when `set +u``
  eval "if [ -z \${init_string_$init_string_hash+x} ]; then init_string_undefined=1; else init_string_undefined=0; fi"
  if [ "$init_string_undefined" -eq 1 ]; then
    unpack_string "$init_string_str"
    : $(( init_string_$((init_string_hash)) = unpack_string_addr ))
    : $(( $init_string_return_var = unpack_string_addr ))
  else
    : $(( $init_string_return_var = init_string_$((init_string_hash)) ))
  fi
}

# djb2 hash algorithm from http://www.cse.yorku.ca/~oz/hash.html
# The hash is truncated to 32 bits to fit in most Shell integers.
djb2() {
  djb2_str=$1
  djb2_hash=5381
  djb2_c=0
  while [ -n "$djb2_str" ] ; do
    djb2_char="$djb2_str"                 # remember current buffer
    djb2_rest="${djb2_str#?}"             # remove the first char
    djb2_char="${djb2_char%"$djb2_rest"}" # remove all but first char
    djb2_str="${djb2_str#?}"              # remove the current char from $src_buf
    char_to_int "$djb2_char"
    : $(( djb2_hash = (((djb2_hash << 5) + djb2_hash) + char_to_int_code) & 4294967295 )) # 2^32 - 1
  done
}

# Primitives

_putchar() { printf \\$(($1/64))$(($1/8%8))$(($1%8)) ; }

_exit() { echo \"Exiting with code $1\"; exit $1; }

_malloc() {
  if [ $# -eq 2 ]; then
    malloc_return_loc=$1; shift
  else
    malloc_return_loc=
  fi
  malloc_size=$1
  strict_alloc $malloc_size
  prim_return_value $strict_alloc_res $malloc_return_loc
}

_free() { return; }

_printf() {
  printf_fmt_ptr=$1; shift
  printf_mod=0
  while [ "$((_$printf_fmt_ptr))" -ne 0 ] ; do
    printf_head="$((_$printf_fmt_ptr))"
    int_to_char "$printf_head"; printf_head_char=$int_to_char_char
    printf_fmt_ptr=$((printf_fmt_ptr + 1))
    if [ $printf_mod -eq 1 ] ; then
      case $printf_head_char in
        'd') # 100 = 'd' Decimal integer
          printf_imm=$1; shift
          printf "%d" "$printf_imm"
          ;;
        'c') # 99 = 'c' Character
          printf_char=$1; shift
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          int_to_char "$printf_char"
          printf "%c" "$int_to_char_char"
          ;;
        'x') # 120 = 'x' Hexadecimal integer
          printf_imm=$1; shift
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          printf "%x" "$printf_imm"
          ;;
        's') # 115 = 's' String
          printf_str_ref=$1; shift
          print_string "$printf_str_ref"
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

          printf_head="$((_$printf_fmt_ptr))"
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
  # printf "%s" "$str"
}

# We represent file descriptors as strings. That means that modes and offsets do not work.
# These limitations are acceptable since c4.cc does not use them.
# TODO: Packing and unpacking the string is a lazy way of copying a string
_open() {
  if [ $# -eq 3 ]; then
    open_return_loc=$1; shift
  else
    open_return_loc=
  fi
  pack_string "$1"
  unpack_string "$pack_string_res"
  prim_return_value $unpack_string_addr $open_return_loc
}

_read() {
  if [ $# -eq 4 ]; then
    read_return_loc=$1; shift
  else
    read_return_loc=
  fi
  read_fd=$1
  read_buf=$2
  read_count=$3
  pack_string "$read_fd"
  read_n_char $read_count $read_buf < "$pack_string_res" # We don't want to use cat because it's not pure Shell
  prim_return_value $read_n_char_len $read_return_loc
}

# File descriptor is just a string, nothing to close
_close() {
  if [ $# -eq 2 ]; then
    close_return_loc=$1; shift
  else
    close_return_loc=
  fi
  prim_return_value 0 $close_return_loc
  return
}

# Used to implement the read instruction.
# Does not work with NUL characters.
read_n_char() {
  read_n_char_count=$1
  read_n_char_buf_ptr=$2
  read_n_char_len=0
  while [ "$read_n_char_count" != "0" ] ; do
    get_char
    case "$get_char_char" in
      EOF) break ;;
      NEWLINE) read_n_char_code=10 ;; # 10 == '\n'
      *) char_to_int "$get_char_char"; read_n_char_code=$char_to_int_code ;;
    esac

    : $((_$read_n_char_buf_ptr=$read_n_char_code))
    : $((read_n_char_buf_ptr += 1))
    : $((read_n_char_count -= 1))
    : $((read_n_char_len += 1))
  done
}

# Read the file, and return a file descriptor to the file. The file descriptor is
# just a string, nothing to close.
_fopen() {
  if [ $# -eq 3 ]; then
    fopen_return_loc=$1; shift
  else
    fopen_return_loc=
  fi
  pack_string "$1"

  fopen_fd=$ALLOC                          # Allocate new FD
  : $((_$((fopen_fd)) = 0))                # Initialize cursor to 0
  fopen_buffer=$((fopen_fd + 1))           # Buffer starts after cursor
  read_all_char $fopen_buffer < "$pack_string_res"
  : $((_$((fopen_buffer + read_all_char_len))=0)) # Terminate buffer with NUL character
  ALLOC=$((ALLOC + read_all_char_len + 2)) # Update ALLOC to the new end of the heap
  prim_return_value $fopen_fd $fopen_return_loc
}

_fclose() {
  if [ $# -eq 2 ]; then
    fclose_return_loc=$1; shift
  else
    fclose_return_loc=
  fi
  _free $1 # Release file descriptor buffer
  prim_return_value 0 $fclose_return_loc
}

_fread() {
  if [ $# -eq 4 ]; then
    fread_return_loc=$1; shift
  else
    fread_return_loc=
  fi
  fread_fd=$1
  fread_buf=$2
  fread_count=$3
  fread_len=0
  fread_fd_buffer=$((fread_fd + 1)) # Buffer starts at fd + 1
  while [ "$fread_count" != "0" ] ; do
    : $((_$fread_buf=_$fread_fd_buffer))
    : $((fread_buf += 1))
    : $((fread_fd_buffer += 1))
    : $((fread_count -= 1))
    : $((fread_len += 1))
  done
  # Update cursor
  : $((_$fread_fd = $fread_len))
  prim_return_value $fread_len $fread_return_loc
}

_fgetc() { # $1: File descriptor
  if [ $# -eq 2 ]; then
    fgetc_return_loc=$1; shift
  else
    fgetc_return_loc=
  fi
  fgetc_fd=$1
  fgetc_cursor=$((_$fgetc_fd))
  fgetc_fd_buffer=$((fgetc_fd + 1)) # Buffer starts at fd + 1
  fgetc_char=$((_$((fgetc_fd_buffer + fgetc_cursor))))
  : $((_$fgetc_fd += 1)) # Update cursor
  prim_return_value $fgetc_char $fgetc_return_loc
}

read_all_char() {
  read_all_char_buf_ptr=$1
  read_all_char_len=0
  while : ; do
    get_char
    case "$get_char_char" in
      EOF) break ;;
      NEWLINE) read_all_char_code=10 ;; # 10 == '\n'
      *) char_to_int "$get_char_char"; read_all_char_code=$char_to_int_code ;;
    esac

    : $((_$read_all_char_buf_ptr=$read_all_char_code))
    : $((read_all_char_buf_ptr += 1))
    : $((read_all_char_len += 1))
  done
}

# Read a character from stdin.
# Uses a buffer for the line, and a smaller fixed-sized buffer to speed up
# reading of long lines. The fixed-sized buffer is refilled from the line buffer
# when it is empty.
# This optimization is necessary because the string operations of all shell
# scale linearly with the size of the string, meaning that removing the head
# char from a string of length N takes O(N) time. Since get_char is called for
# every character in the line, reading a line of length N takes O(N^2) time.
# This optimization doesn't change the complexity but reduces the constant
# factor.
get_char_src_buf=""
get_char_src_buf_fast=""
get_char_fast_buffer_len=50
get_char()                           # get next char from source into $char
{
  if [ -z "$get_char_src_buf_fast" ] && [ -z "$get_char_src_buf" ] ; then
    IFS=                             # don't split input
    if read -r get_char_src_buf ; then        # read next line into $src_buf
      if [ -z "$get_char_src_buf" ] ; then    # an empty line implies a newline character
        get_char_char=NEWLINE                 # next get_char call will read next line
        return
      fi
      get_char_src_buf_fast=$(printf "%.${get_char_fast_buffer_len}s" "$get_char_src_buf") # Copy first 100 chars to fast buffer
      get_char_src_buf="${get_char_src_buf#"$get_char_src_buf_fast"}"       # remove first 100 chars from $src_buf
    else
      get_char_char=EOF                       # EOF reached when read fails
      return
    fi
  elif [ -z "$get_char_src_buf_fast" ] || [ -z "${get_char_src_buf_fast#?}" ]; then      # need to refill fast buffer
    get_char_src_buf_fast=$(printf "%.${get_char_fast_buffer_len}s" "$get_char_src_buf") # Copy first 100 chars to fast buffer
    get_char_src_buf="${get_char_src_buf#"$get_char_src_buf_fast"}"       # remove first 100 chars from $src_buf
    if [ -z "$get_char_src_buf_fast" ] ; then           # end of line if the buffer is now empty
      get_char_char=NEWLINE
      return
    fi
    get_char_char=$get_char_src_buf_fast                    # remember current buffer
    get_char_rest=${get_char_src_buf_fast#?}                # remove the first char
    get_char_char=${get_char_char%"$get_char_rest"}                  # remove all but first char
  else # src_buf_fast is not empty and doesn't contain only a newline
    get_char_src_buf_fast="${get_char_src_buf_fast#?}"      # remove the current char from $src_buf
  fi

  # current character is at the head of $src_buf_fast
  get_char_char=$get_char_src_buf_fast                    # remember current buffer
  get_char_rest=${get_char_src_buf_fast#?}                # remove the first char
  get_char_char=${get_char_char%"$get_char_rest"}                  # remove all but first char
}

_memset() {
  if [ $# -eq 4 ]; then
    memset_return_loc=$1; shift
  else
    memset_return_loc=
  fi
  memset_ptr=$1
  memset_val=$2
  memset_len=$3
  memset_ix=0
  while [ $memset_ix -lt $memset_len ]; do
    : $((_$((memset_ptr + memset_ix)) = memset_val))
    : $((memset_ix += 1))
  done
  prim_return_value $memset_ptr $memset_return_loc
}

_memcmp() {
  if [ $# -eq 4 ]; then
    memcmp_return_loc=$1; shift
  else
    memcmp_return_loc=
  fi
  memcmp_op1=$1
  memcmp_op2=$2
  memcmp_len=$3
  memcmp_ix=0
  memcmp_a=0
  while [ $memcmp_ix -lt $memcmp_len ]; do
    if [ $((_$((memcmp_op1 + memcmp_ix)))) -ne $((_$((memcmp_op2 + memcmp_ix)))) ] ; then
      # From man page: returns the difference between the first two differing bytes (treated as unsigned char values
      prim_return_value $((_$((memcmp_op1 + memcmp_ix)) - _$((memcmp_op2 + memcmp_ix)))) $memcmp_return_loc
      return
    fi
    : $((memcmp_ix = memcmp_ix + 1))
  done
  prim_return_value 0 $memcmp_return_loc
}

# Debug

_show_heap() {
  set +u
  show_heap_ix=1
  show_heap_elided=0
  echo "    Heap:"
  while [ $show_heap_ix -lt $ALLOC ]; do
    show_heap_location="_$show_heap_ix"
    # Safe way of checking if the variable is defined or not. With +u, we could also check if it's empty.
    eval "if [ -z \${$show_heap_location+x} ]; then show_heap_undefined=1; else show_heap_undefined=0; fi"
    if [ "$show_heap_undefined" -eq 1 ]; then
      show_heap_elided=1
    else
      if [ "$show_heap_elided" -eq 1 ]; then
        echo "        ..."
        show_heap_elided=0
      fi

      show_heap_ascii=$((_$show_heap_ix))
      show_heap_char=""
      if [ $show_heap_ascii -ge 31 ] && [ $show_heap_ascii -le 127 ] ; then
        int_to_char "$show_heap_ascii"
        show_heap_char=$int_to_char_char
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
  while [ $show_arg_stack_ix -le $((SP + 1)) ]; do
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
