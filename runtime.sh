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
