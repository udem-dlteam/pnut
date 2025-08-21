#! /bin/sh
#
# Shell implementation of the cat command.
# This implementation can only read text files as the shell cannot read \0 chars.
#
# Usage: cat [file ...]

posix_cat() {
  IFS=
  while read -r line; do
    printf "%s\n" "$line"
  done

  # Print the last line if it's not empty (for files with no trailing newline)
  if [ -n "$line" ]; then
    printf "%s" "$line"
  fi
}

cat() {
  for file in "$@"; do
    if [ -f "$file" ]; then
      posix_cat < "$file"
    else
      printf 'cat: %s: No such file\n' "$file"
    fi
  done
}

cat "$@"
