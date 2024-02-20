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
src_buf=
src_buf_fast=
get_char()                           # get next char from source into $char
{
  if [ -z "$src_buf_fast" ] && [ -z "$src_buf" ] ; then
    IFS=                             # don't split input
    if read -r src_buf ; then        # read next line into $src_buf
      if [ -z "$src_buf" ] ; then    # an empty line implies a newline character
        char=NEWLINE                 # next get_char call will read next line
        return
      fi
      src_buf_fast=$(printf "%.40s" "$src_buf") # Copy first 100 chars to fast buffer
      src_buf="${src_buf#"$src_buf_fast"}"       # remove first 100 chars from $src_buf
    else
      char=EOF                       # EOF reached when read fails
      return
    fi
  elif [ -z "$src_buf_fast" ] || [ -z "${src_buf_fast#?}" ]; then      # need to refill fast buffer
    src_buf_fast=$(printf "%.40s" "$src_buf") # Copy first 100 chars to fast buffer
    src_buf="${src_buf#"$src_buf_fast"}"       # remove first 100 chars from $src_buf
    if [ -z "$src_buf_fast" ] ; then           # end of line if the buffer is now empty
      char=NEWLINE
      return
    fi
    char=$src_buf_fast                    # remember current buffer
    rest=${src_buf_fast#?}                # remove the first char
    char=${char%"$rest"}                  # remove all but first char
  else # src_buf_fast is not empty and doesn't contain only a newline
    src_buf_fast="${src_buf_fast#?}"      # remove the current char from $src_buf
  fi

  # current character is at the head of $src_buf_fast
  char=$src_buf_fast                    # remember current buffer
  rest=${src_buf_fast#?}                # remove the first char
  char=${char%"$rest"}                  # remove all but first char
}

read_n_char() {
  count=$1
  acc=""
  while [ "$count" != "0" ] ; do
    get_char
    acc="$acc$char"
    : $((count--))
  done

  # echo $acc
}

read_n_char $1 < $2
