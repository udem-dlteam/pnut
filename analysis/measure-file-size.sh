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
mkdir -p "$TEMP_DIR/no-sys"

expand_includes() { # $1 = output-name, $2 = options
  ./$TEMP_DIR/pnut-includes pnut.c $2 | \
    # Filter lines ending with "// INCLUDED"
    # Those lines correspond to #include directives that have been expanded
    sed "/\/\/ INCLUDED$/d" > "$TEMP_DIR/$1.c"

  # As a sanity check, we make sure that the expanded file still compiles with gcc
  gcc -o "$TEMP_DIR/$1" "$TEMP_DIR/$1.c" $2 -Wall -Werror

  # and that the result of pnut on the expanded file is the same as the result of pnut on the original file
  # If the options contain -DBOOTSTRAP_PNUT, use pnut-sh-bootstrap instead
  if echo "$2" | grep -q "\-DPNUT_BOOTSTRAP"; then
    ./$TEMP_DIR/pnut-sh-bootstrap pnut.c $2 > "$TEMP_DIR/$1.sh"
    ./$TEMP_DIR/pnut-sh-bootstrap "$TEMP_DIR/$1.c" $2 > "$TEMP_DIR/$1-preincluded.sh"
    ./$TEMP_DIR/pnut-awk-bootstrap pnut.c $2 > "$TEMP_DIR/$1.awk"
    ./$TEMP_DIR/pnut-awk-bootstrap "$TEMP_DIR/$1.c" $2 > "$TEMP_DIR/$1-preincluded.awk"
  else
    ./$TEMP_DIR/pnut-sh pnut.c $2 > "$TEMP_DIR/$1.sh"
    ./$TEMP_DIR/pnut-sh "$TEMP_DIR/$1.c" $2 > "$TEMP_DIR/$1-preincluded.sh"
    ./$TEMP_DIR/pnut-awk pnut.c $2 > "$TEMP_DIR/$1.awk"
    ./$TEMP_DIR/pnut-awk "$TEMP_DIR/$1.c" $2 > "$TEMP_DIR/$1-preincluded.awk"
  fi

  # Because we use the __FILE__ macro in pnut, the preincluded.sh file will have
  # a different path than the original file. We need to replace the path in the
  # preincluded file with the path of the original file.
  # Note: | is used as the delimiter because the path contains /
  cat "$TEMP_DIR/$1-preincluded.sh"  | sed "s|$TEMP_DIR/$1.c|pnut.c|" > "$TEMP_DIR/$1-preincluded-canonical.sh"
  cat "$TEMP_DIR/$1-preincluded.awk" | sed "s|$TEMP_DIR/$1.c|pnut.c|" > "$TEMP_DIR/$1-preincluded-canonical.awk"

  diff -q "$TEMP_DIR/$1.sh" "$TEMP_DIR/$1-preincluded-canonical.sh" || \
    { echo "Error: $1.sh and $1-preincluded-canonical.sh differ"; exit 1; }

  diff -q "$TEMP_DIR/$1.awk" "$TEMP_DIR/$1-preincluded-canonical.awk" || \
    { echo "Error: $1.awk and $1-preincluded-canonical.awk differ"; exit 1; }
}

# C source files contain comments and other things that inflate the file size
# without necessarily contributing to code size. This function removes these
# comments and whitespace to get a more accurate measure of the code size.
clean_file() { # $1 = input file, $2 = output file
  cat "$1" | \
    # Remove comments
    sed "s/\/\/.*//g" | \
    sed "s/\/\*.*\*\///g" | \
    # Remove trailing whitespace
    sed 's/[[:space:]]*$//g' | \
    # Remove empty lines
    sed '/^[[:space:]]*$/d' > "$2"
}

lines_ratio() { # $1 = numerator, $2 = denominator
  printf "%d/%d = %s\n" $1 $2 $(echo "scale=3; $1 / $2" | bc -l)
}

measure_size() { # $1 = output-name, $2 = options
  expand_includes "$1" "$2"

  files="$(gcc -MM pnut.c $2 | sed 's/^[^:]*: //')"
  cleaned_files=""
  for file in $files; do
    # Make cleaned copy of the file, without comments or blank lines
    clean_file "$file" "$TEMP_DIR/$file"
    cleaned_files="$cleaned_files $TEMP_DIR/$file"

    # Make copies of the files without system includes, these will be passed to
    # the gcc preprocessor to get a line count without system headers
    sed '/#include </d' "$file" > "$TEMP_DIR/no-sys/$file"
  done

  # Preprocess pnut.c using gcc, so we have a line count without any comments,
  # blank lines, and preprocessor directives.
  gcc -E -P "build/measure/no-sys/pnut.c" $2 > "$TEMP_DIR/$1-preprocessed.c"
  preprocessed_lines=$(wc -l < "$TEMP_DIR/$1-preprocessed.c")
  # Split file in 2 parts, before "#_ Character constants" and after
  marker_line=$(grep -n "^#_ Character constants" "$TEMP_DIR/$1.sh" | cut -d: -f1)
  tail -n +$marker_line "$TEMP_DIR/$1.sh" > "$TEMP_DIR/$1-runtime.sh"
  head -n $((marker_line - 1)) "$TEMP_DIR/$1.sh" > "$TEMP_DIR/$1-decls.sh"

  printf "########## $1 ##########\n"
  printf "By file:\n"
  wc $files
  printf "By file (without comments or blank lines):\n"
  wc $cleaned_files
  printf "Expanded includes:\n"
  wc "$TEMP_DIR/$1.c" "$TEMP_DIR/$1.sh" "$TEMP_DIR/$1.awk" "$TEMP_DIR/$1-preprocessed.c" "$TEMP_DIR/$1-runtime.sh" "$TEMP_DIR/$1-decls.sh"

  # Number of empty lines in $TEMP_DIR/$1.sh
  printf "Ratio (Original):     "; lines_ratio "$(wc -l < $TEMP_DIR/$1.sh)" "$(wc -l < $TEMP_DIR/$1.c)"
  printf "Ratio (Cleaned):      "; lines_ratio "$(wc -l < $TEMP_DIR/$1.sh)" "$(wc -l $cleaned_files | tail -n 1 | awk '{print $1}')"
  printf "Ratio (Preprocessed): "; lines_ratio "$(wc -l < $TEMP_DIR/$1.sh)" "$preprocessed_lines"
  printf "Empty lines count:    %d\n" "$(grep -c '^[[:space:]]*$' "$TEMP_DIR/$1-decls.sh")"
  printf "Runtime size:         %d\n" "$(wc -l < "$TEMP_DIR/$1-runtime.sh")"

  printf "\n"
}

# Compile pnut in a mode that tokenizes the input and expands active #include directives
gcc -o "$TEMP_DIR/pnut-includes" pnut.c -DDEBUG_EXPAND_INCLUDES
# Compile pnut-sh
gcc -o "$TEMP_DIR/pnut-sh" pnut.c -Dtarget_sh
# Compile pnut-sh-bootstrap
gcc -o "$TEMP_DIR/pnut-sh-bootstrap" pnut.c -Dtarget_sh -DPNUT_BOOTSTRAP
# Compile pnut-awk
gcc -o "$TEMP_DIR/pnut-awk" pnut.c -Dtarget_awk
# Compile pnut-awk-bootstrap
gcc -o "$TEMP_DIR/pnut-awk-bootstrap" pnut.c -Dtarget_awk -DPNUT_BOOTSTRAP

# Measuring for pnut-sh
measure_size "pnut-sh" "-Dtarget_sh"
measure_size "pnut-minimal-sh" "-Dtarget_sh -DPNUT_BOOTSTRAP"

# Measuring for pnut-awk
measure_size "pnut-awk" "-Dtarget_awk"
measure_size "pnut-minimal-awk" "-Dtarget_awk -DPNUT_BOOTSTRAP"

# ...and for the other targets
# measure_size "pnut-i386_linux" "-Dtarget_i386_linux"
# measure_size "pnut-minimal-i386_linux" "-Dtarget_i386_linux -DPNUT_BOOTSTRAP"
# measure_size "pnut-i386_linux-one-pass" "-Dtarget_i386_linux -DONE_PASS_GENERATOR"
measure_size "pnut-minimal-i386_linux-one-pass" "-Dtarget_i386_linux -DONE_PASS_GENERATOR -DPNUT_BOOTSTRAP"
measure_size "pnut-minimal-x86_64_linux-one-pass" "-Dtarget_x86_64_linux -DONE_PASS_GENERATOR -DPNUT_BOOTSTRAP"
measure_size "pnut-minimal-x86_64_mac-one-pass" "-Dtarget_x86_64_mac -DPNUT_BOOTSTRAP"
