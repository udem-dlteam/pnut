#! /bin/sh
#
# Shell implementation of the touch command.
#
# Usage: touch [file ...]

touch() {
  while [ $# -gt 0 ]; do
    if [ ! -e "$1" ]; then
      # Create the file if it does not exist
      printf "" > "$1"
    fi
    shift 1
  done

  return 0
}

touch "$@"
