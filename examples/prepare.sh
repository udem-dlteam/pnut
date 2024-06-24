#! /bin/sh

echo "Compiling examples"

PNUT_SH_OPTIONS="-DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -DRT_COMPACT -Dsh -DSH_INCLUDE_C_CODE"

# Compile pnut.exe
gcc -o pnut-sh.exe $PNUT_SH_OPTIONS pnut.c

for file in $(find examples -type f -name "*.c" | sort); do
  filename=$(basename $file .c);
  echo "Compiling $filename"
  ./pnut-sh.exe $file > examples/$filename.sh
done
