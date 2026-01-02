set -e -u

TEMP_DIR="build/compile-times"
COMP_DIR="$TEMP_DIR/compiled"

# Create the compiled directory if it doesn't exist
mkdir -p $TEMP_DIR
mkdir -p $COMP_DIR

print_time()
{
  ms=$1
  printf "%s %s\n" "$((ms/1000)).$((ms/100%10))$((ms/10%10))$((ms%10))s" "$2"
}

measure_time() {
  bash -c "time $1" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"
}

# Options passed to pnut-sh (mostly include paths or macro definitions)
compile_options() {
  echo `sed -n -e "/\/\/ comp-options:/p" "$1" | sed -e "s/^\/\/ comp-options://" |  tr '\n' ',' | sed -e 's/,$//'`
}

compile() {
  runner=$1
  file=$2
  name=$(basename $2 ".c")
  options="${3-}"
  output_name=${4-$name}

  if [ $1 = "gcc" ]; then
    TIME_MS=$(measure_time "$COMP_DIR/pnut-sh-base.exe $file $options $(compile_options $file) > $COMP_DIR/$output_name-with-$runner.sh")
  elif [ $runner = "pnut" ]; then
    TIME_MS=$(measure_time "$COMP_DIR/pnut-sh-compiled-by-pnut-exe.exe $file $options $(compile_options $file) > $COMP_DIR/$output_name-with-$runner.sh")
  else
    TIME_MS=$(measure_time "$runner $COMP_DIR/pnut.sh $file $options $(compile_options $file) > $COMP_DIR/$output_name-with-$runner.sh")
  fi
  print_time $TIME_MS "for: $runner with $file $(sha256sum $COMP_DIR/$output_name-with-$runner.sh | cut -d' ' -f1) size: $(wc -l < $file) $(wc -l < $COMP_DIR/$output_name-with-$runner.sh)"
}

PNUT_SH_OPTIONS="-Dtarget_sh"
PNUT_EXE_OPTIONS="-Dtarget_i386_linux -DONE_PASS_GENERATOR"
gcc -o $COMP_DIR/pnut-sh-base.exe $PNUT_SH_OPTIONS -O3 pnut.c
gcc -o $COMP_DIR/pnut-exe.exe $PNUT_EXE_OPTIONS -O3 pnut.c
./$COMP_DIR/pnut-sh-base.exe $PNUT_SH_OPTIONS pnut.c > $COMP_DIR/pnut.sh
./$COMP_DIR/pnut-exe.exe $PNUT_SH_OPTIONS pnut.c > $COMP_DIR/pnut-sh-compiled-by-pnut-exe.exe
chmod +x $COMP_DIR/pnut-sh-compiled-by-pnut-exe.exe

programs="examples/empty.c examples/hello.c examples/fib.c examples/cat.c examples/cp.c examples/wc.c examples/sha256sum.c examples/c4.c examples/repl.c"
runners="ksh dash bash yash zsh gcc pnut"

for prog in $programs; do
  for run_with in $runners; do
    compile "$run_with" $prog
  done
done

# Compile pnut-sh.c using pnut-sh on different shells/compilers
for run_with in $runners; do
  compile "$run_with" "pnut.c" "$PNUT_SH_OPTIONS" "pnut-sh"
done

# Compile pnut-exe.c using pnut-sh on different shells/compilers
for run_with in $runners; do
  compile "$run_with" "pnut.c" "$PNUT_EXE_OPTIONS" "pnut-exe"
done
