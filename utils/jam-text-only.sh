#! /bin/sh
# Jam utility: Create self-extracting POSIX shell archives containing text files.
# Example usage:
#   ./jam.sh file1 file2 dir1 dir2

EOF_SEP="EOF3141592653"

# Replace every non-alphanumeric character with _
normalize_name() { # $1: name to normalize
  name="${1}"
  normalized_name=""
  while [ -n "$name" ]; do
    char="${name%"${name#?}"}" # Get the first character
    name="${name#?}"           # Remove the first character
    case "$char" in
      [a-zA-Z0-9]) normalized_name="${normalized_name}${char}" ;;
      *)           normalized_name="${normalized_name}_" ;;
    esac
  done
  printf "%s" "$normalized_name"
}

# POSIX shell implementation of the cat utility
pcat() {
  IFS=
  while read -r line; do
    printf "%s\n" "$line"
  done
}

# Add pcat function to output
gen_pcat() {
  pcat << 'EOF'
pcat() {
  IFS=
  while read -r line; do
    printf "%s\n" "$line"
  done
}

EOF
}

process_file() { # $1: file to process, $2: path
  normalized_name="$(normalize_name "$1")"
  printf "extract_%s() {\n" "$normalized_name"
  printf "  printf \"Extracting %s\\\\n\"\n" "$1"
  printf "  pcat << '%s' > %s\n" "$EOF_SEP" "$1"
  pcat "$1" < "$1"
  printf "%s\n}\n\n" "$EOF_SEP"
  printf "extract_%s\n" "$normalized_name"
}

process_dir() {
  printf "Processing directory '$1'.\n"
  IFS=" "
  for file in "$1"/*; do
    if [ -f "$file" ]; then
      process_file "$file"
    elif [ -d "$file" ]; then
      process_dir "$file"
    fi
  done
}

# For each file/directory argument, add its content to the archive
for arg in "$@"; do
  if [ -f "$arg" ]; then
    process_file "$arg"
  elif [ -d "$arg" ]; then
    process_dir "$arg"
  fi
done
