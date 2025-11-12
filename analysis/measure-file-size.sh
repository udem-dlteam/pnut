#! /bin/bash
#
# With pnut split across multiple files with many compilation targets and
# options, the size of the compiler is not trivial to measure. gcc's -E option
# can be used to expand the preprocessor directives, but it also includes all
# the code from the #include <> directives which is not code pnut sees and so we
# need something in between that only expands the active #include "" directives.
#
# This script uses the DEBUG_EXPAND_INCLUDES option that preprocess the input
# and prints the characters that are read. Active #include directive are
# expanded meaning that the included file is printed as well. The output is then
# filtered to remove the lines that correspond to the included files.
#
# The result is as if someone had manually replaced the active #include
# directives with the content of the included files.
#
# We then run a few tests to make sure the result is correct:
# - The expanded file still compiles with gcc
# - The result of pnut on the expanded file is the same as the result of pnut on the original file

set -e # Exit on error

TEMP_DIR="build/measure"
mkdir -p "$TEMP_DIR"

expand_includes() { # $1 = output-name, $2 = options
  ./$TEMP_DIR/pnut-includes.exe pnut.c $2 | \
    # Filter lines ending with "// INCLUDED"
    # Those lines correspond to #include directives that have been expanded
    sed "/\/\/ INCLUDED$/d" > "$TEMP_DIR/$1.c"

  # As a sanity check, we make sure that the expanded file still compiles with gcc
  gcc -o "$TEMP_DIR/$1.exe" "$TEMP_DIR/$1.c" $2 -Wall -Werror

  # and that the result of pnut on the expanded file is the same as the result of pnut on the original file
  ./$TEMP_DIR/pnut-sh.exe pnut.c $2 > "$TEMP_DIR/$1.sh"
  ./$TEMP_DIR/pnut-sh.exe "$TEMP_DIR/$1.c" $2 > "$TEMP_DIR/$1-preincluded.sh"

  # Because we use the __FILE__ macro in pnut, the preincluded.sh file will have
  # a different path than the original file. We need to replace the path in the
  # preincluded file with the path of the original file.
  # Note: | is used as the delimiter because the path contains /
  cat "$TEMP_DIR/$1-preincluded.sh" | sed "s|$TEMP_DIR/$1.c|pnut.c|" > "$TEMP_DIR/$1-preincluded-canonical.sh"

  diff -q "$TEMP_DIR/$1.sh" "$TEMP_DIR/$1-preincluded-canonical.sh" || \
    { echo "Error: $1.sh and $1-preincluded-canonical.sh differ"; exit 1; }
}

included_files() {
  ./$TEMP_DIR/pnut-includes.exe pnut.c $2 | \
    # Filter lines ending with "// INCLUDED"
    # Those lines correspond to #include directives that have been expanded
    sed -n "/\/\/ INCLUDED$/p" | \
    # Extract the file name from the line
    sed -n "s/.*\"\(.*\)\".*/\1/p"
}

# C source files contain comments and other things that inflate the file size
# without necessarily contributing to code size. This function removes these
# comments and whitespace to get a more accurate measure of the code size.
clean_file() { # $1 = input file
  cat "$1" | \
    # Remove comments
    sed "s/\/\/.*//g" | \
    sed "s/\/\*.*\*\///g" | \
    # Remove trailing whitespace
    sed 's/[[:space:]]*$//g' | \
    # Remove empty lines
    sed '/^[[:space:]]*$/d'
}

lines_ratio() { # $1 = numerator, $2 = denominator
  printf "%d/%d = %s\n" $1 $2 $(echo "scale=3; $1 / $2" | bc -l)
}

measure_size() { # $1 = output-name, $2 = options
  expand_includes "$1" "$2"

  files="$(included_files "$1" "$2")"
  files="pnut.c $files" # Add pnut.c because it's not included directly
  cleaned_files=""
  for file in $files; do
    clean_file "$file" > "$TEMP_DIR/$file"
    cleaned_files="$cleaned_files $TEMP_DIR/$file"
  done

  printf "########## $1 ##########\n"
  # echo "Files included in $1: $(echo $files | tr '\n' ' ')"
  printf "By file:\n"
  # Measure the size of the cleaned files
  wc $files
  printf "By file (without comments or blank lines):\n"
  wc $cleaned_files
  printf "Expanded includes:\n"
  wc "$TEMP_DIR/$1.c"
  wc "$TEMP_DIR/$1.sh"
  printf "Ratio (Original): "; lines_ratio "$(wc -l < $TEMP_DIR/$1.sh)" "$(wc -l < $TEMP_DIR/$1.c)"
  printf "Ratio (Cleaned):  "; lines_ratio "$(wc -l < $TEMP_DIR/$1.sh)" "$(wc -l $cleaned_files | tail -n 1 | awk '{print $1}')"

  printf "\n"
}

# Compile pnut in a mode that tokenizes the input and expands active #include directives
gcc -o "$TEMP_DIR/pnut-includes.exe" pnut.c -DDEBUG_EXPAND_INCLUDES
# Compile pnut-sh
gcc -o "$TEMP_DIR/pnut-sh.exe" pnut.c -Dsh

# Measuring for pnut-sh
measure_size "pnut-sh" "-Dsh"
measure_size "pnut-minimal-sh" "-Dsh -DPNUT_BOOTSTRAP"

# ...and for the other targets
measure_size "pnut-i386_linux" "-Dtarget_i386_linux"
measure_size "pnut-minimal-i386_linux" "-Dtarget_i386_linux -DPNUT_BOOTSTRAP"
measure_size "pnut-i386_linux-one-pass" "-Dtarget_i386_linux -DONE_PASS_GENERATOR"
measure_size "pnut-minimal-i386_linux-one-pass" "-Dtarget_i386_linux -DONE_PASS_GENERATOR -DPNUT_BOOTSTRAP"
# measure_size "pnut-x86_64_linux" "-Dtarget_x86_64_linux"
# measure_size "pnut-x86_64_mac" "-Dtarget_x86_64_mac"
