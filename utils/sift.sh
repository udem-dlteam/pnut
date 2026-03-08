#! /bin/sh
# sift.sh: Extract files in a jammed archive without running it.
# Example usage:
#   ./sift.sh pnut.c < jammed.sh

set -e -u
error() { printf "Error: %s\n" "$1" >&2; exit 1; }

if [ $# -lt 1 ]; then error "Usage: $0 file_path < archive_file"; fi
file="$1"
EOF_MARKER="EOF3141592653"

# Read until we find a line containing "> $file"
while IFS= read -r line; do
  [ "$line" != "${line#*> $file}" ] && break
done

# Then read lines until we find the EOF_MARKER line, printing them out
while IFS= read -r line; do
  [ "$line" = "$EOF_MARKER" ] && break
  printf "%s\n" "$line"
done

# If we reached EOF without finding the marker, the file was not found
if [ "$line" != "$EOF_MARKER" ]; then
  error "File '$file' not found in archive."
fi
