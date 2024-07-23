#! /bin/sh

DIR="examples/"
COMP_DIR="$DIR/compiled"

mkdir -p $COMP_DIR

echo "Compiling examples"

PNUT_SH_OPTIONS="-DRELEASE_PNUT_SH"

# Compile pnut.exe
gcc -o pnut-sh.exe $PNUT_SH_OPTIONS pnut.c

for file in $(find examples -type f -name "*.c" | sort); do
  filename=$(basename $file .c);
  echo "Compiling $filename"
  ./pnut-sh.exe $file > $COMP_DIR/$filename.sh
  chmod +x $COMP_DIR/$filename.sh
done
