set -e -u

DIR="benchmarks/compile-times"
COMP_DIR="$DIR/compiled"

# Create the compiled directory if it doesn't exist
mkdir -p $COMP_DIR

print_time()
{
  ms=$1
  printf "%s %s\n" "$((ms/1000)).$((ms/100%10))$((ms/10%10))$((ms%10))s" "$2"
}

comp_program_options() { # $1 = program
  case $1 in
    "examples/c4.c") echo "-I./examples/c4-libs/" ;;
  esac
}

compile() {
  # shell=$1
  file=$2
  name=$(basename $2 ".c")
  options="${3-}"
  output_name=${4-$name}

  if [ $1 = "gcc" ]; then
    TIME_MS=$(( `bash -c "time $COMP_DIR/pnut-sh-base.exe $file $options $(comp_program_options $file) > $COMP_DIR/$output_name-with-$1.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  elif [ $1 = "pnut" ]; then
    TIME_MS=$(( `bash -c "time $COMP_DIR/pnut-sh-compiled-by-pnut-exe.exe $file $options $(comp_program_options $file) > $COMP_DIR/$output_name-with-$1.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  else
    TIME_MS=$(( `bash -c "time $1 $COMP_DIR/pnut.sh $file $options $(comp_program_options $file) > $COMP_DIR/$output_name-with-$1.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  fi
  print_time $TIME_MS "for: $1 with $file $(sha256sum $COMP_DIR/$output_name-with-$1.sh | cut -d' ' -f1) size: $(wc -l < $file) $(wc -l < $COMP_DIR/$output_name-with-$1.sh)"
}

PNUT_SH_OPTIONS="-Dtarget_sh"
PNUT_x86_OPTIONS="-Dtarget_i386_linux"
#PNUT_x86_OPTIONS="-Dtarget_x86_64_linux"
#PNUT_x86_OPTIONS="-Dtarget_x86_64_mac"
gcc -o $COMP_DIR/pnut-sh-base.exe $PNUT_SH_OPTIONS -O3 pnut.c
gcc -o $COMP_DIR/pnut-exe.exe $PNUT_x86_OPTIONS -O3 pnut.c
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
  compile "$run_with" "pnut.c" "-Dtarget_sh" "pnut-sh"
done

# Compile pnut-exe.c using pnut-sh on different shells/compilers
for run_with in $runners; do
  compile "$run_with" "pnut.c" "-Dtarget_i386_linux" "pnut-exe"
done
