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
    unpack_string_char="$unpack_string_src_buf"                    # remember current buffer
    unpack_string_rest="${unpack_string_src_buf#?}"                # remove the first char
    unpack_string_char="${unpack_string_char%"$unpack_string_rest"}"             # remove all but first char
    unpack_string_src_buf="${unpack_string_src_buf#?}"             # remove the current char from $src_buf
    unpack_string_code=$(LC_CTYPE=C printf "%d" "'$unpack_string_char'")
    push_data "$unpack_string_code"
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
      *) pack_string_res=$pack_string_res$(printf "\\$(printf "%o" "$pack_string_char")") # Decode
    esac
  done
}

# Emit a C-string character by character so that printf doesn't interpret % and \ as format specifiers.
print_string() {
  print_string_addr="$1";  shift
  print_string_max_len=100000000
  print_string_delim=0
  print_string_len=0
  print_string_res=""
  if [ $# -ge 1 ] ; then print_string_delim="$1" ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then print_string_max_len="$1" ; shift ; fi # Optional max length
  while [ "$((_$print_string_addr))" -ne $print_string_delim ] && [ $print_string_max_len -gt $print_string_len ] ; do
    print_string_char="$((_$print_string_addr))"
    print_string_addr=$((print_string_addr + 1))
    print_string_len=$((print_string_len + 1))
    case $print_string_char in
      10) printf "\n" ;; # 10 == '\n'
      *) printf "\\$(printf "%o" "$print_string_char")" ;; # Decode
    esac
  done
}

# Local variables

SP=0 # Note: Stack grows up, not down

_result_loc=0 # This is useful when value-return-method != '(addr #t))
save_loc_var() {
  : $((SP += 1))
  unset "_data_$SP" # For some reason, ksh doesn't like to overwrite the value
  eval "_data_$SP='$_result_loc'"
  while [ $# -gt 0 ]; do
    : $((SP += 1))
    : $((_data_$SP=$1))
    shift
  done
}

rest_loc_var() {
  while [ $# -gt 0 ]; do
    : $(($1=_data_$SP))
    : $((SP -= 1))
    shift
  done
  eval "_result_loc=\$_data_$SP"
  : $((SP -= 1))
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
    printf_head_char=$(printf "\\$(printf "%o" "$printf_head")") # Decode
    printf_fmt_ptr=$((printf_fmt_ptr + 1))
    if [ $printf_mod -eq 1 ] ; then
      case $printf_head_char in
        'd') # 100 = 'd' Decimal integer
          printf_imm=$1; shift
          printf "%d" "$printf_imm"
          # str="$str$imm"
          ;;
        'c') # 99 = 'c' Character
          printf_char=$1; shift
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          printf "\\$(printf "%o" "$printf_char")"
          # str="$str$(printf "\\$(printf "%o" "$char")")"
          ;;
        'x') # 120 = 'x' Hexadecimal integer
          printf_imm=$1; shift
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          printf "%x" "$printf_imm"
          # str="$str$(printf "%x" "$imm")"
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
            # str="$str$pack_string_res"
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
        10) printf "\n" ;; # str="$str\n" ;; # 10 == '\n'
        37) printf_mod=1 ;; # 37 == '%'
        *) printf "$printf_head_char" ;; # str="$str$head_char" ;; # Decode
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
      *) read_n_char_code=$(LC_CTYPE=C printf "%d" "'$get_char_char") # convert to integer code ;;
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
  : $((_$((fopen_buffer + read_all_char_len + 1))=0)) # Terminate buffer with NUL character
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
  # echo "fgetc"
  # _show_fd $fgetc_fd
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
      *) read_all_char_code=$(LC_CTYPE=C printf "%d" "'$get_char_char") # convert to integer code ;;
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
  elided=0
  echo "    Heap:"
  while [ $show_heap_ix -lt $ALLOC ]; do
    location="_$show_heap_ix"
    eval "if [[ -z \${$location+x} ]]; then undefined=1; else undefined=0; fi"
    if [[ $undefined -eq 1 ]]; then
      elided=1
    else
      if [ $elided -eq 1 ]; then
        echo "        ..."
        elided=0
      fi

      show_heap_ascii=$((_$show_heap_ix))
      show_heap_char=""
      if [ $show_heap_ascii -ge 31 ] && [ $show_heap_ascii -le 127 ] ; then
        show_heap_char=$(printf "\\$(printf "%o" "$show_heap_ascii")")
      fi
      echo "        _$show_heap_ix = $show_heap_ascii  ($show_heap_char)"
    fi
    : $((show_heap_ix += 1))
  done
  set -u
}

_show_arg_stack() {
  set +u
  arg_ix=1
  echo "    Heap:"
  while [ $arg_ix -le $((SP + 1)) ]; do
    eval "val=\$_data_$arg_ix"
    echo "        _$arg_ix = $val"
    : $((arg_ix += 1))
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
