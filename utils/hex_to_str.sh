#! /bin/sh
#
# hex_to_str: decode pairs of hexadecimal digits read from stdin into characters.
#
# Example: echo "48 65 6c 6c 6f 20 77 6f 72 6c 64 21" | ./hex_to_str.sh

line_to_str() {
  for word in $1; do
    word=$((0x$word + 0)) # Convert hex to decimal
    printf \\$(($word/64))$(($word/8%8))$(($word%8))
  done
}

accum=""
while read -r word; do
  accum="$accum $word"
done

line_to_str "$accum"
printf "\n"
