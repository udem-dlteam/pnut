set -e -u

TEMP_DIR="build/example-execution-times"
COMP_DIR="$TEMP_DIR/compiled"

# Create the compiled directory if it doesn't exist
mkdir -p "$TEMP_DIR"
mkdir -p "$COMP_DIR"

programs="examples/c4.c examples/repl.c"
runners="pnut"

fail() { echo "$1"; exit $2; }

print_time()
{
  ms=$1
  printf "%s %s\n" "$((ms/1000)).$((ms/100%10))$((ms/10%10))$((ms%10))s" "$2"
}

measure_time() {
  bash -c "time $1" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"
}

# Options passed when generating pnut-sh (to customize shell code generation)
pnut_compile_options() {
  echo `sed -n -e "/\/\/ pnut-options:/p" "$1" | sed -e "s/^\/\/ pnut-options://" |  tr '\n' ',' | sed -e 's/,$//'`
}

# Options passed to pnut-sh (mostly include paths or macro definitions)
compile_options() {
  echo `sed -n -e "/\/\/ comp-options:/p" "$1" | sed -e "s/^\/\/ comp-options://" |  tr '\n' ',' | sed -e 's/,$//'`
}

run_program_options() { # $1 = program, $2 = runner
  case $1 in
    "examples/cat.c")       echo "$COMP_DIR/file-64k" ;;
    "examples/cp.c")
      touch "$COMP_DIR/file-64k-out-$2" # gcc and pnut only write to the file if it exists
      echo "$COMP_DIR/file-64k $COMP_DIR/file-64k-out-$2"
      ;;
    "examples/wc.c")        echo "$COMP_DIR/file-64k" ;;
    "examples/sha256sum.c") echo "$COMP_DIR/file-64k" ;;
    "examples/c4.c")        echo "examples/c4.c" ;;
    "examples/repl.c")      echo "< $COMP_DIR/program.scm" ;;
    *)                      echo "" ;;
  esac
}

run() {
  runner="$1"
  name=$(basename $2 ".c")
  comp_options="${3-}"
  run_options="${4-}"

  if [ $runner = "gcc" ]; then
    executable="$COMP_DIR/$name-gcc.exe"
    gcc -o "$executable" -O3 $2 $comp_options 2> /dev/null || fail "Error: Failed to compile $2"
    TIME_MS=$(measure_time "./$executable $run_options > $COMP_DIR/$name-output-with-$runner")
  elif [ $runner = "pnut" ]; then
    executable="$COMP_DIR/$name-pnut.exe"
    ./$COMP_DIR/pnut-exe $2 $comp_options -o $executable
    chmod +x $executable
    TIME_MS=$(measure_time "./$executable $run_options > $COMP_DIR/$name-output-with-$runner")
  else
    TIME_MS=$(measure_time "$runner $COMP_DIR/$name.sh $run_options > $COMP_DIR/$name-output-with-$runner")
  fi
  print_time $TIME_MS "for: $runner with $name on $comp_options/$run_options $(sha256sum $COMP_DIR/$name-output-with-$runner | cut -d' ' -f1)"
}

# Generate 64k file
i=0
rm -f "$COMP_DIR/file-64k"
while [ $i -lt 128 ] ; do
  printf "%s\n" "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&()*+,-./:;<=>?@[]^_{|}~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&()*+,-./:;<=>?@[]^_{|}~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&()*+,-./:;<=>?@[]^_{|}~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&()*+,-./:;<=>?@[]^_{|}~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&()*+,-./:;<=>?@[]^_{|}~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123" >> $COMP_DIR/file-64k;
  : $(( i += 1 ))
done;

# and program.scm
# (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)
# (define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1))))) (factorial 10)
# (display "Hello, world!")
echo '(display "Hello, world!")' > $COMP_DIR/program.scm

PNUT_SH_OPTIONS="-Dsh -DRT_NO_INIT_GLOBALS"
# BOOTSTRAP_LONG so long long are mapped to 32-bit integers (long long is used in c4)
# ALLOW_RECURSIVE_MACROS so repl.c can be compiled to work around a recursive macro expansion bug
PNUT_EXE_OPTIONS="-Dtarget_i386_linux -DONE_PASS_GENERATOR -DBOOTSTRAP_LONG -DALLOW_RECURSIVE_MACROS"
gcc -o $COMP_DIR/pnut-sh-base.exe $PNUT_SH_OPTIONS -O3 pnut.c 2> /dev/null || fail "Error: Failed to compile pnut-sh"
gcc -o $COMP_DIR/pnut-exe $PNUT_EXE_OPTIONS -O3 pnut.c 2> /dev/null || fail "Error: Failed to compile pnut-exe"

for prog in $programs; do
  filename=$(basename $prog .c);
  pnut_opts=$(pnut_compile_options $prog)
  file_opts=$(compile_options $prog)

  printf "Compiling $filename with $pnut_opts ${file_opts:+"$file_opts "}\n"
  # Compile pnut-sh with specific options
  gcc -o "$TEMP_DIR/pnut-sh-for-$filename.exe" $PNUT_SH_OPTIONS $pnut_opts pnut.c 2> /dev/null || fail "Error: Failed to compile pnut with $pnut_opts"
  # Then generate the executable
  ./$TEMP_DIR/pnut-sh-for-$filename.exe "$prog" $file_opts > $COMP_DIR/$filename.sh
  chmod +x $COMP_DIR/$filename.sh

  for runner in $runners; do
    run "$runner" $prog "$file_opts" "$(run_program_options $prog $runner)"
  done
done
