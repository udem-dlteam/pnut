#! /bin/sh

# wc implemented in shell, to compare with pnut implementation

# Usage: wc < file1

lines=0
words=0
chars=0

while IFS= read -r line; do
  : $((lines += 1))
  : $((chars += ${#line} + 1))

  # IFS=" "
  # for word in $line; do
  #   # echo "word: '$word'"
  #   : $(( words += 1 ))
  # done
  space_prev=0
  while [ -n "$line" ]; do
    case $line in
      # Match on sequence of spaces
      ' '*|'\t'*|'\n'*)
        if [ $space_prev -eq 0 ]; then
          : $(( words += 1 ))
          space_prev=1
        fi
        line="${line#?}"
        # echo "line: '$line'"
        ;;
      *) space_prev=0; line="${line#?}" ;;
    esac
  done
done

echo $lines $words $chars
