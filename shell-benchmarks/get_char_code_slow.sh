src_buf_slow=
get_char_slow()                           # get next char from source into $char
{
  if [ -z "$src_buf_slow" ] ; then        # need to get next line when buffer empty
    IFS=                             # don't split input
    if read -r src_buf_slow ; then        # read next line into $src_buf
      if [ -z "$src_buf_slow" ] ; then    # an empty line implies a newline character
        char=NEWLINE                 # next get_char call will read next line
        return
      fi
    else
      char=EOF                       # EOF reached when read fails
      return
    fi
  else
    src_buf_slow="${src_buf_slow#?}"           # remove the current char from $src_buf
    if [ -z "$src_buf_slow" ] ; then      # end of line if the buffer is now empty
      char=NEWLINE
      return
    fi
  fi

  # current character is at the head of $src_buf

  char="$src_buf_slow"                    # remember current buffer
  rest="${src_buf_slow#?}"                # remove the first char
  char="${char%"$rest"}"             # remove all but first char
}

read_n_char_slow() {
  count=$1
  acc=""
  while [ "$count" != "0" ] ; do
    get_char_slow
    acc="$acc$char"
    : $((count--))
  done

  # echo $acc
}

read_n_char_slow $1 < $2
