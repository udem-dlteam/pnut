#!/bin/sh

DIR="examples/"
COMP_DIR="$DIR/compiled"

mkdir -p $COMP_DIR

echo "Compiling examples"

PNUT_SH_OPTIONS="-DRELEASE_PNUT_SH -DRT_COMPACT"

gcc -o build/pnut-sh-base.exe $PNUT_SH_OPTIONS pnut.c # Compile pnut.exe

compile_options() {
  echo `sed -n -e "/\/\/ pnut-options:/p" "$1" | sed -e "s/^\/\/ pnut-options://" |  tr '\n' ',' | sed -e 's/,$//'`
}

for file in $(find examples -type f -name "*.c" | sort); do
  filename=$(basename $file .c);
  file_opts=$(compile_options $file)
  echo "Compiling $filename with $file_opts"
  # To speed up the compilation process, we only compile pnut.exe if there are specific options
  if [ ! -z "$file_opts" ]; then
    gcc -o build/pnut-sh-opt.exe $PNUT_SH_OPTIONS $file_opts pnut.c # Compile pnut.exe with specific options
    ./build/pnut-sh-opt.exe $file $file_opts > $COMP_DIR/$filename.sh
  else
    ./build/pnut-sh-base.exe $file > $COMP_DIR/$filename.sh
  fi
  chmod +x $COMP_DIR/$filename.sh
done
