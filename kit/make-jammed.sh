set -e -u

WITH_TAR_GZ=0

while [ $# -gt 0 ]; do
  case $1 in
    --with-tcc-tar-gz)    WITH_TAR_GZ="1"; shift 1 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

program_dependencies() {
  file="$1"
  comp_options="$2"

  echo $(gcc -MM "$file" $comp_options | tr ':' '\n' | tr '\\' ' ' | sed '1d')
}

FILES_TO_INCLUDE="
kit/pnut-sh.sh:pnut-sh.sh
$(program_dependencies "pnut.c" "-Dtarget_i386_linux -DSAFE_MODE -DBOOTSTRAP_TCC")
kit/bintools.c:bintools.c
portable_libc/include/fcntl.h:fcntl.h
portable_libc/include/math.h:math.h
portable_libc/include/pnut_lib.h:pnut_lib.h
portable_libc/include/setjmp.h:setjmp.h
portable_libc/include/stdio.h:stdio.h
portable_libc/include/stdlib.h:stdlib.h
portable_libc/include/string.h:string.h
portable_libc/include/sys/stat.h:stat.h
portable_libc/include/sys/types.h:types.h
portable_libc/include/unistd.h:unistd.h
portable_libc/include/stdarg.h:stdarg.h
portable_libc/src/math.c:math.c
portable_libc/src/pnut_lib.c:pnut_lib.c
portable_libc/src/setjmp.c:setjmp.c
portable_libc/src/stdio.c:stdio.c
portable_libc/src/stdlib.c:stdlib.c
portable_libc/src/string.c:string.c
kit/bintools-libc.c:bintools-libc.c
kit/bootstrap.sh:bootstrap.sh
utils/cat.sh:cat.sh

portable_libc/include
portable_libc/src
portable_libc/libc.c
kit/tcc-patches
kit/libtcc1.c
kit/config.h

"

if [ $WITH_TAR_GZ -eq 1 ]; then
  FILES_TO_INCLUDE="$FILES_TO_INCLUDE kit/tcc-0.9.26.tar.gz"
else
  tar -xzf kit/tcc-0.9.26.tar.gz
  FILES_TO_INCLUDE="$FILES_TO_INCLUDE tcc-0.9.26-1147-gee75a10c"
fi

FILES=""    # Paths of all files added to jam archive
JAM_ARGS="" # Paths of all files to include (with path override)

traverse_dir() {
  dir=$1

  if [ -d "$dir" ]; then
    for file in "$dir"/*; do
      traverse "$file"
    done
  fi
}

traverse() { # $1: file, $2: extraction path (optional), $3: parent directory substitution (optional)
  file=$1

  if [ -d "$file" ]; then
    traverse_dir "$file"
  elif [ -f "$file" ]; then

    # If the file is a .tar.gz archive, open it and recurse on its content
    # case "$file" in
    #   *.tar.gz)
    #     if [ $EXTRACT_ARCHIVES -eq 1 ]; then
    #       echo "Extracting $file"
    #       tmp_dir=$(mktemp -d)
    #       mkdir -p "$tmp_dir"
    #       tar -xzf "$file" -C "$tmp_dir"
    #       traverse_dir "$tmp_dir" "" "$tmp_dir:"
    #     else
    #       # Just a simple file, add it
    #       FILES="$FILES $file"
    #       JAM_ARGS="$JAM_ARGS $file:${2:+:$2}"
    #     fi
    #     ;;
    #     ;;
    # esac
    #   *)

        # Just a simple file, add it
        JAM_ARGS="$JAM_ARGS $file${2:+:$2}"
        FILES="$FILES $file"
  else
    printf "Skipping '$file': not a file or directory.\n"
  fi
}

for file in $FILES_TO_INCLUDE; do
  path=${file##*:}
  if [ "$path" != "$file" ]; then
    traverse "${file%:*}" "$path"
  else
    traverse "$file"
  fi
done

cat << 'EOF' > kit/jammed.sh
#! /bin/sh
#
# Check that the script is executable.
# This is so the bootstrap script can steal its execute bit, since it's
# impossible to bootstrap the execute bit from the shell.
if [ ! -x "$0" ] && [ "$1" != "--force-no-exec" ]; then
  printf "Error: $0 is not executable.\n"
  printf "This script is intended to be run in a specific environment where it can execute successfully.\n"
  printf "Please ensure you are in an empty directory and have the necessary permissions.\n"
  printf "Run \`chmod +x $0\` and try again.\n"
  exit 126
fi

EOF

./utils/jam.sh $JAM_ARGS >> kit/jammed.sh

# Evaluate disk usage:
jammed_size=$(wc -c kit/jammed.sh | awk '{print $1}')

# Compare to the sum of each file
files_size=$(wc -c $FILES | grep total | awk '{print $1}')

echo "kit/jammed.sh: $jammed_size bytes"
echo "Individual files size: $files_size bytes"
echo "Ratio: $(echo "scale=3; $jammed_size / $files_size" | bc -l)"
