push_data() {
  : $((_$ALLOC=$1))
  : $((ALLOC += 1))
}

unpack_array() {
  addr=$ALLOC
  while [ $# -gt 0 ] ; do
    push_data $1
    shift
  done
}

# Push a Shell string to the VM heap. Returns a reference to the string in $addr.
unpack_string() {
  addr=$ALLOC
  src_buf="$1"
  while [ -n "$src_buf" ] ; do
    char="$src_buf"                    # remember current buffer
    rest="${src_buf#?}"                # remove the first char
    char="${char%"$rest"}"             # remove all but first char
    src_buf="${src_buf#?}"             # remove the current char from $src_buf
    code=$(LC_CTYPE=C printf "%d" "'$char'")
    push_data "$code"
  done
  push_data 0
}

# Convert a VM string reference to a Shell string.
# $res is set to the result, and $len is set to the length of the string.
pack_string() {
  addr="$1";  shift
  max_len=100000000
  delim=0
  if [ $# -ge 1 ] ; then delim="$1" ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then max_len="$1" ; shift ; fi # Optional max length
  len=0; res=""
  while [ "$((_$addr))" -ne $delim ] && [ $max_len -gt $len ] ; do
    char="$((_$addr))"
    addr=$((addr + 1))
    len=$((len + 1))
    case $char in
      10) res="$res\n" ;; # 10 == '\n'
      *) res=$res$(printf "\\$(printf "%o" "$char")") # Decode
    esac
  done
}

_printf() {
  fmt_ptr=$1; shift
  mod=0
  while [ "$((_$fmt_ptr))" -ne 0 ] ; do
    head="$((_$fmt_ptr))"
    head_char=$(printf "\\$(printf "%o" "$head")") # Decode
    fmt_ptr=$((fmt_ptr + 1))
    if [ $mod -eq 1 ] ; then
      case $head_char in
        'd') # 100 = 'd' Decimal integer
          imm=$1; shift
          printf "%d" "$imm"
          # str="$str$imm"
          ;;
        'c') # 99 = 'c' Character
          char=$1; shift
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          printf "\\$(printf "%o" "$char")"
          # str="$str$(printf "\\$(printf "%o" "$char")")"
          ;;
        'x') # 120 = 'x' Hexadecimal integer
          imm=$1; shift
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          printf "%x" "$imm"
          # str="$str$(printf "%x" "$imm")"
          ;;
        's') # 115 = 's' String
          str_ref=$1; shift
          pack_string "$str_ref" # result in $res
          # Note: Using printf "%s" "$res" outputs "\n" if $res contains a newline
          # but printf "$res" seems to work fine.
          printf "$res"
          # str="$str$res"
          ;;
        '.') # String with length
          pack_string $fmt_ptr 0 2 # Read next 2 characters
          fmt_ptr=$((fmt_ptr + 2))
          if [ "$res" = "*s" ]; then
            len=$1; shift
            str_ref=$1; shift
            pack_string $str_ref 0 $len # result in $res
            printf "%s" "$res"
            # str="$str$res"
          else
            echo "Unknown format specifier: %.$res" ; exit 1
          fi
          ;;
        [0-9])                         # parse integer
          # Get max length (with padding)
          pack_string $fmt_ptr 46 # Read until '.' or end of string
          fmt_ptr=$((fmt_ptr + len + 1))
          min_len="$head_char$res" # Don't forget the first digit we've already read

          # Get string length
          pack_string $fmt_ptr 115 # Read until 's' or end of string
          fmt_ptr=$((fmt_ptr + len))
          str_len=$res

          head="$((_$fmt_ptr))"
          head_char=$(printf "\\$(printf "%o" "$head")") # Decode
          fmt_ptr=$((fmt_ptr + 1))
          if [ "$head_char" = 's' ]; then
            str_ref=$1; shift
            pack_string $str_ref 0 $str_len # result in $res
              : $((padding_len = $min_len - $len))
            while [ $padding_len -gt 0 ]; do # Pad string so it has at least $min_len characters
              res=" $res"
                : $((padding_len -= 1))
              done
            printf "%s" "$res"
            # str="$str$res"
          else
            echo "Unknown format specifier: '%$min_len.$str_len$head_char'" ; exit 1;
          fi
          ;;
        *)
          echo "Unknown format specifier %$head_char"; exit 1
      esac
      mod=0
    else
      case $head in
        10) printf "\n" ;; # str="$str\n" ;; # 10 == '\n'
        37) mod=1 ;; # 37 == '%'
        *) printf "$head_char" ;; # str="$str$head_char" ;; # Decode
      esac
    fi
  done
  # printf "%s" "$str"
}
