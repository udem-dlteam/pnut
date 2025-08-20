#! /bin/sh
#
# Shell implementation of the cat command.
# This implementation can only read text files as the shell cannot read \0 chars.
#
# Usage: cat [file ...]

cat() {
  for file in "$@"; do
    if [ -f "$file" ]; then
      echo "#### $file ####\n" # Extra newline for better readability
      while IFS= read -r line; do
        printf '%s\n' "$line"
      done < "$file"
    else
      printf 'cat: %s: No such file\n' "$file"
    fi
  done
}

cat "$@"
