#! /bin/sh
# This script processes the user include directives (#include "") in the source
# files, replacing them with the actual content of the included files.
#
# GCC can be used to preprocess the files, but this script is designed to
# handle the includes in a more controlled manner, especially for custom
# projects where the include paths may not be standard.
#
# Usage: ./process-includes.sh <source-file>

# Log function to print messages to stderr
log() {
  printf "%s\n" "$1" >&2
}

# Initialize a variable to keep track of system includes
# This will prevent duplicate system includes from being processed.
system_includes=""

# Check if the system include has already been processed.
# If not, add it to the list of system includes.
# Returns 0 if already included, 1 if newly added.
system_include_already_included() {
  if echo "$system_includes" | grep -q "$1"; then
    return 0
  else
    system_includes="$system_includes $1"
    return 1
  fi
}

# Compute base path for user include
include_base_path() { # $1: included file, $2: base path
  # Get the directory part of the base path
  file_base_dir="$(dirname "$1")"
  if [ -z "$file_base_dir" ] || [ "$file_base_dir" = "." ]; then
    # If the base path is empty or just ".", return the second argument
    # This means the included file is in the same directory as the source file.
    echo "$2"
    return
  else
    echo "$2/$file_base_dir"
  fi
}

# Process the user includes of stdin
process_file() { # $1: file to process, $2: relative path to the source file
  while IFS= read -r line; do
    case $line in
      # System include, remove it unless we've seen it before
      "#include <"*">" )
        file="${line#\#include\ <}"
        file="${file%>}"
        if ! system_include_already_included "$file"; then
          printf "%s\n" "$line"
        fi
        ;;

      # User include, replace it with the content of the included file and recursively process it
      "#include \""*"\"" )
        # Extract the included file path
        included_file="${line#\#include\ \"}"
        included_file="${included_file%\"}"
        log "Processing user include: $(include_base_path "$included_file" "$2")/$included_file"
        # Process the included file
        process_file "$included_file" "$(include_base_path "$included_file" "$2")"
        ;;

      # Other lines, print as is
      * )
        printf "%s\n" "$line"
        ;;
    esac
  done < "$2/$1"
}

# Check if a file is provided
if [ $# -eq 0 ]; then
  printf "Usage: $0 file.c\n"
  exit 1
fi

process_file "$(basename "$1")" "$(dirname "$1")"
