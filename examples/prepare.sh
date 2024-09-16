#!/bin/sh

set -e -u

DIR="examples/"
COMP_DIR="$DIR/compiled"

mkdir -p $COMP_DIR
mkdir -p build

echo "Compiling examples"

PNUT_SH_OPTIONS="-DRELEASE_PNUT_SH -DRT_COMPACT"

# Compile pnut.exe
gcc -o build/pnut-sh-base.exe $PNUT_SH_OPTIONS pnut.c 2> /dev/null || fail "Error: Failed to compile pnut"

pnut_compile_options() {
  echo `sed -n -e "/\/\/ pnut-options:/p" "$1" | sed -e "s/^\/\/ pnut-options://" |  tr '\n' ',' | sed -e 's/,$//'`
}

compile_options() {
  echo `sed -n -e "/\/\/ comp-options:/p" "$1" | sed -e "s/^\/\/ comp-options://" |  tr '\n' ',' | sed -e 's/,$//'`
}

fail() { echo "$1"; exit $2; }

failed=0

generate_executable_with() { # $1 = executable, $2 = options
  if [ $# = 1 ]; then
    opt=""
  else
    opt=$2
    printf "with $opt "
  fi

  if ./build/$1 $file $opt > $COMP_DIR/$filename.sh; then
    chmod +x $COMP_DIR/$filename.sh
    printf "✅\n"
  else
    printf "Failed to compile ❌\n"
    failed=1
  fi
}

for file in $(find examples -type f -name "*.c" | sort); do
  filename=$(basename $file .c);
  pnut_opts=$(pnut_compile_options $file)
  file_opts=$(compile_options $file)
  echo "Compiling $filename with pnut options: $pnut_opts and file options: $file_opts"

  # To speed up the compilation process, we only compile pnut.exe if there are specific options
  if [ -z "$pnut_opts" ]; then
    printf "Compiling $filename: "
    generate_executable_with "pnut-sh-base.exe" "$file_opts"
  else
    printf "Compiling $filename with $pnut_opts: "
    # Compile pnut.exe with specific options
    gcc -o build/pnut-sh-opt.exe $PNUT_SH_OPTIONS $pnut_opts pnut.c 2> /dev/null || fail "Error: Failed to compile pnut with $pnut_opts"
    generate_executable_with "pnut-sh-opt.exe" "$file_opts"
  fi
done

if [ $failed -eq 1 ]; then
  echo "##### Some examples failed to compile #####"
  exit 1
fi
