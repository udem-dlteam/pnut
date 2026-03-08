#! /bin/bash
#
# With pnut split across multiple files with many compilation targets and
# options, the size of the compiler is not trivial to measure. gcc's -E option
# can be used to expand the preprocessor directives, but it also includes all
# the code from the #include <> directives which is not code pnut sees. To solve
# this, we make copies of the source files with system includes removed which
# are passed to gcc -E -P to get a line count without system headers.
#
# We then run a few tests to make sure the result is correct:
# - The expanded file still compiles with gcc
# - The result of pnut on the expanded file is the same as the result of pnut on the original file

set -e # Exit on error

TEMP_DIR="build/measure"
mkdir -p "$TEMP_DIR"
mkdir -p "$TEMP_DIR/no-sys"

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
  # and that the result of pnut on the expanded file is the same as the result of pnut on the original file
  # If the options contain -DBOOTSTRAP_PNUT, use pnut-sh-bootstrap instead
  if echo "$2" | grep -q "\-DPNUT_BOOTSTRAP"; then
    ./$TEMP_DIR/pnut-sh-bootstrap pnut.c $2 > "$TEMP_DIR/$1.sh"
    ./$TEMP_DIR/pnut-awk-bootstrap pnut.c $2 > "$TEMP_DIR/$1.awk"
  else
    ./$TEMP_DIR/pnut-sh pnut.c $2 > "$TEMP_DIR/$1.sh"
    ./$TEMP_DIR/pnut-awk pnut.c $2 > "$TEMP_DIR/$1.awk"
  fi

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
  # Split file in 2 parts, before "#_ Character constants" and after
  marker_line=$(grep -n "^#_ Character constants" "$TEMP_DIR/$1.sh" | cut -d: -f1)
  tail -n +$marker_line "$TEMP_DIR/$1.sh" > "$TEMP_DIR/$1-runtime.sh"
  head -n $((marker_line - 1)) "$TEMP_DIR/$1.sh" > "$TEMP_DIR/$1-decls.sh"

  printf "########## $1 ##########\n"
  printf "By file:\n"
  wc $files
  printf "By file (without comments or blank lines):\n"
  wc $cleaned_files
  printf "Preprocessed (gcc -E -P without system includes):\n"
  wc "$TEMP_DIR/$1.sh" "$TEMP_DIR/$1.awk" "$TEMP_DIR/$1-preprocessed.c" "$TEMP_DIR/$1-runtime.sh" "$TEMP_DIR/$1-decls.sh"

  # Number of empty lines in $TEMP_DIR/$1.sh
  sh_lines=$(wc -l < "$TEMP_DIR/$1.sh")
  c_lines=$(wc -l $files | tail -n 1 | awk '{print $1}')
  cleaned_lines=$(wc -l $cleaned_files | tail -n 1 | awk '{print $1}')
  preprocessed_lines=$(wc -l < "$TEMP_DIR/$1-preprocessed.c")

  printf "Ratio (Original):     "; lines_ratio "$sh_lines" "$c_lines"
  printf "Ratio (Cleaned):      "; lines_ratio "$sh_lines" "$cleaned_lines"
  printf "Ratio (Preprocessed): "; lines_ratio "$sh_lines" "$preprocessed_lines"
  printf "Empty lines count:    %d\n" "$(grep -c '^[[:space:]]*$' "$TEMP_DIR/$1-decls.sh")"
  printf "Runtime size:         %d\n" "$(wc -l < "$TEMP_DIR/$1-runtime.sh")"

  printf "\n"
}

# Compile pnut-sh
gcc -o "$TEMP_DIR/pnut-sh" pnut.c -Dtarget_sh
# Compile pnut-sh-bootstrap
gcc -o "$TEMP_DIR/pnut-sh-bootstrap" pnut.c -Dtarget_sh -DPNUT_BOOTSTRAP
# Compile pnut-awk
gcc -o "$TEMP_DIR/pnut-awk" pnut.c -Dtarget_awk
# Compile pnut-awk-bootstrap
gcc -o "$TEMP_DIR/pnut-awk-bootstrap" pnut.c -Dtarget_awk -DPNUT_BOOTSTRAP

# Measure the minimal pnut variants:

measure_size "pnut-minimal-sh" "-Dtarget_sh -DPNUT_BOOTSTRAP"
measure_size "pnut-minimal-awk" "-Dtarget_awk -DPNUT_BOOTSTRAP"
measure_size "pnut-minimal-i386_linux-one-pass" "-Dtarget_i386_linux -DONE_PASS_GENERATOR -DPNUT_BOOTSTRAP"
# measure_size "pnut-minimal-x86_64_linux-one-pass" "-Dtarget_x86_64_linux -DONE_PASS_GENERATOR -DPNUT_BOOTSTRAP"
# measure_size "pnut-minimal-x86_64_mac-one-pass" "-Dtarget_x86_64_mac -DPNUT_BOOTSTRAP"

# Measure the complete pnut variants:
measure_size "pnut-complete-sh" "-Dtarget_sh"
measure_size "pnut-awk" "-Dtarget_awk"
measure_size "pnut-complete-i386_linux-one-pass" "-Dtarget_i386_linux -DONE_PASS_GENERATOR"
