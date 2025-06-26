#! /bin/sh
#
# Shell implementation of the ls command.
# Usage: ls {file_or_directory}

if [ $# -eq 0 ]; then
  dir="." # No arguments, list current directory
else
  dir="$1"
fi

if [ ! -d "$dir" ]; then
  printf 'ls: %s: No such directory\n' "$dir"
  return 1
fi

for file in "$dir"/*; do
  if [ -e "$file" ]; then
    printf '%s\n' "$file"
  fi
done
