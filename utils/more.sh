#! /bin/sh
# more.sh: Basic pager that displays text one screen (20 lines) at a time.
# Example usage:
#   ./more.sh some_file

set -e -u
error() { printf "Error: %s\n" "$1" >&2; exit 1; }

if [ $# -lt 1 ]; then error "Usage: $0 file_path"; fi

LINES_PER_PAGE=20

block_on_user_input() {
  IFS= read -r user_input                  # Wait for user input on stdin
  if [ -n "$user_input" ]; then exit 0; fi # Exit on any non-empty input
}

exec 3< "$1" # Open file for reading on fd 3
line_count=0
while IFS= read -r line <&3; do
  printf "%s\n" "$line"
  : $(( line_count += 1 ))
  if [ $line_count -ge $LINES_PER_PAGE ]; then
    block_on_user_input
    line_count=0
  fi
done
