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
