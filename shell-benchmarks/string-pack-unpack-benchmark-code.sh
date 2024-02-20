# set -x
set -e
set -u

dat=0
push_data() {
  : $((_data_$dat=$1))
  : $((dat += 1))
}

push_string() {
  addr=$dat
  src_buf="$1"
  while [ -n "$src_buf" ] ; do
    char="$src_buf"                    # remember current buffer
    rest="${src_buf#?}"                # remove the first char
    char="${char%"$rest"}"             # remove all but first char
    src_buf="${src_buf#?}"             # remove the current char from $src_buf
    code=$(LC_CTYPE=C printf "%d" "'$char'")
    push_data "$code"
  done
  push_data 0 # null terminator
}

rehydrate_string() {
  addr="$1"
  str=""
  while [ "$((_data_$addr))" != "0" ] ; do
    char="$((_data_$addr))"
    addr=$((addr + 1))
    str=$str$(printf "\\$(printf "%o" "$char")") # Decode
  done
}

push_string "abcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyzabcdefhijklmnopqrstuvwxyz"
rehydrate_string $addr
# echo "$str"
