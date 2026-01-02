#! /bin/sh
# skim.sh: Extract files in a jammed archive, without running the jammed script.
# Example usage:
#   ./skim.sh --archive jammed.sh pnut.c
#
# Options:
#   --archive: Specify the jammed archive file path (required)
#   --eof: Specify a custom end-of-file separator (default is EOF3141592653)

set -e -u

error() { printf "Error: %s\n" "$1" >&2; exit 1; }

if [ $# -lt 4 ]; then error "Usage: $0 --archive <archive_file> --extract <file_path> [--eof CUSTOM_EOF]"; fi

ARCHIVE_FILE=""
FILE_TO_EXTRACT=""
CUSTOM_EOF="EOF3141592653"
while [ $# -gt 0 ]; do
  case "$1" in
    --archive) ARCHIVE_FILE="$2";    shift 2 ;;
    --extract) FILE_TO_EXTRACT="$2"; shift 2 ;;
    --eof)     CUSTOM_EOF="$2";      shift 2 ;;
    *) error "Unknown option '$1'" ;;
  esac
done

# Read until we find a line containing "> $FILE_TO_EXTRACT"
# Then read lines until we find the CUSTOM_EOF line
{
  while IFS= read -r line; do
    [ "$line" != "${line#*> $FILE_TO_EXTRACT}" ] && break
  done

  while IFS= read -r line; do
    [ "$line" = "$CUSTOM_EOF" ] && break
    printf "%s\n" "$line"
  done
} < "$ARCHIVE_FILE"

if [ "$line" != "$CUSTOM_EOF" ]; then
  error "File '$FILE_TO_EXTRACT' not found in archive '$ARCHIVE_FILE'."
fi
