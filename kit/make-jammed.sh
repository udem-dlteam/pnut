#! /bin/sh
# make-jammed.sh: Create the jammed.sh shell archive script containing all
#                 required files for bootstrapping pnut and beyond!
# Example usage:
#   ./kit/make-jammed.sh
#
# Options:
#   -b, --binary: Pass the --binary option to the jam utility
#   --include-utils: Include some debug scripts in the jammed archive.

set -e -u

error() {
  printf "Error: %s\n" "$1" >&2
  exit 1
}

readonly TEMP_DIR="kit"

JAM_OPT=""
INCLUDE_UTILS=0
WITH_TAR_GZ=0

while [ $# -gt 0 ]; do
  case $1 in
    -b|--binary)        JAM_OPT="$JAM_OPT --binary"; shift 1 ;;
    --include-utils)    INCLUDE_UTILS=1;             shift 1 ;;
    --with-tcc-tar-gz)  WITH_TAR_GZ="1";             shift 1 ;;
    *)               error "Unknown option: $1"           ;;
  esac
done

mkdir -p "$TEMP_DIR"

program_dependencies() {
  file="$1"
  comp_options="$2"

  echo $(gcc -MM "$file" $comp_options | tr ':' '\n' | tr '\\' ' ' | sed '1d')
}

# Prepare pnut-sh.sh
PNUT_SH_OPTIONS="-Dtarget_sh -DPNUT_BOOTSTRAP"
gcc -o "$TEMP_DIR/pnut-sh" $PNUT_SH_OPTIONS pnut.c
./$TEMP_DIR/pnut-sh $PNUT_SH_OPTIONS pnut.c > "$TEMP_DIR/pnut-sh.sh"

FILES_TO_INCLUDE="
$TEMP_DIR/pnut-sh.sh:pnut-sh.sh
$(program_dependencies "pnut.c" "-Dtarget_i386_linux -DBOOTSTRAP_TCC")
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

kit/tcc-patches/array_sizeof.before
kit/tcc-patches/array_sizeof.after
kit/tcc-patches/fix_stack_64_bit_operands_on_32_bit.before
kit/tcc-patches/fix_stack_64_bit_operands_on_32_bit.after
kit/tcc-patches/float_negation.before
kit/tcc-patches/float_negation.after
kit/tcc-patches/sscanf_TCC_VERSION.before
kit/tcc-patches/sscanf_TCC_VERSION.after
kit/tcc-patches/undefine_TCC_IS_NATIVE.before
kit/tcc-patches/undefine_TCC_IS_NATIVE.after

kit/libtcc1.c
kit/config.h
"

if [ $WITH_TAR_GZ -eq 1 ]; then
  FILES_TO_INCLUDE="$FILES_TO_INCLUDE kit/tcc-0.9.26.tar.gz"
else
  tar -xzf kit/tcc-0.9.26.tar.gz
  touch tcc-0.9.26-1147-gee75a10c/config.h
  FILES_TO_INCLUDE="$FILES_TO_INCLUDE $(program_dependencies "tcc-0.9.26-1147-gee75a10c/tcc.c" "-DONE_SOURCE")"
fi

if [ $INCLUDE_UTILS -eq 1 ]; then
FILES_TO_INCLUDE="$FILES_TO_INCLUDE
utils/ls.sh:ls.sh
utils/touch.sh:touch.sh
utils/wc.sh:wc.sh
"
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
    # Just a simple file, add it
    JAM_ARGS="$JAM_ARGS $file${2:+:$2}"
    FILES="$FILES $file"
  else
    printf "Error: '$file' not a file or directory.\n" >&2
    exit 1
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

cat << 'EOF' > "$TEMP_DIR/jammed.sh"
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

./utils/jam.sh $JAM_OPT $JAM_ARGS >> "$TEMP_DIR/jammed.sh"

# Evaluate disk usage:
jammed_size=$(wc -c "$TEMP_DIR/jammed.sh" | awk '{print $1}')

# Compare to the sum of each file
files_size=$(wc -c $FILES | grep total | awk '{print $1}')

printf "%s/jammed.sh: %d bytes\n" $TEMP_DIR $jammed_size
printf "Individual files size: %d bytes\n" $files_size
printf "Ratio: %s\n" "$(printf "scale=3; $jammed_size / $files_size\n" | bc -l)"

# wc $FILES
