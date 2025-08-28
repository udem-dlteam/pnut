#! /bin/sh
#
# wc command but with directory traversal
# Usage: wc [file_or_directory]

traverse_dir() {
  printf "Processing directory '$1'.\n"
  IFS=" "
  for file in "$1"/*; do
    traverse_file "$file"
  done
}

traverse_file() {
  if [ -f "$1" ]; then
    FILES="$FILES $1"
  elif [ -d "$1" ]; then
    traverse_dir "$1"
  fi
}

wc_all_files() {
  FILES=""
  for arg in "$@"; do
    traverse_file "$arg"
  done

  wc $FILES
}

wc_all_files "$@"
