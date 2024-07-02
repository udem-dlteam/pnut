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

compile() {
  # shell=$1
  file=$2
  name=$(basename $2)
  name="${name%.*}"
  options="${3-}"
  output_name=${4-$name}

  if [ $1 = "gcc" ]; then
    TIME_MS=$(( `bash -c "time $COMP_DIR/pnut-sh-base.exe $file $options > $COMP_DIR/$output_name-with-$1.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  elif [ $1 = "pnut" ]; then
    TIME_MS=$(( `bash -c "time $COMP_DIR/pnut-sh-compiled-by-pnut-exe.exe $file $options > $COMP_DIR/$output_name-with-$1.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  else
    TIME_MS=$(( `bash -c "time $1 $COMP_DIR/pnut.sh $file $options > $COMP_DIR/$output_name-with-$1.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  fi
  print_time $TIME_MS "for: $1 with $file"
}

run() {
  name=$(basename $2)
  name="${name%.*}"
  options="${3-}"

  if [ $1 = "gcc" ]; then
    executable="$COMP_DIR/$name-gcc.exe"
    gcc -o "$executable" -O3 $2
    TIME_MS=$(( `bash -c "time ./$executable $options > $COMP_DIR/$name-output-with-$1" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  elif [ $1 = "pnut" ]; then
    executable="$COMP_DIR/$name-pnut.exe"
    ./$COMP_DIR/pnut-exe.exe $2 > $executable
    chmod +x $executable
    TIME_MS=$(( `bash -c "time ./$executable $options > $COMP_DIR/$name-output-with-$1" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  else
    TIME_MS=$(( `bash -c "time $1 $COMP_DIR/$name-with-gcc.sh $options > $COMP_DIR/$name-output-with-$1" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  fi
  print_time $TIME_MS "for: $1 with $name on $options"
}

PNUT_SH_OPTIONS="-DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Dsh"
PNUT_x86_OPTIONS="-DSUPPORT_INCLUDE -Di386"
gcc -o $COMP_DIR/pnut-sh-base.exe $PNUT_SH_OPTIONS -O3 pnut.c
gcc -o $COMP_DIR/pnut-exe.exe $PNUT_x86_OPTIONS -O3 pnut.c
./$COMP_DIR/pnut-sh-base.exe $PNUT_SH_OPTIONS pnut.c > $COMP_DIR/pnut.sh
./$COMP_DIR/pnut-exe.exe $PNUT_SH_OPTIONS pnut.c > $COMP_DIR/pnut-sh-compiled-by-pnut-exe.exe

chmod +x $COMP_DIR/pnut-sh-compiled-by-pnut-exe.exe

# Generate 64k file
i=0
rm -f "$COMP_DIR/file-64k"
while [ $i -lt 128 ] ; do
  printf "%s\n" "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&()*+,-./:;<=>?@[]^_{|}~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&()*+,-./:;<=>?@[]^_{|}~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&()*+,-./:;<=>?@[]^_{|}~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&()*+,-./:;<=>?@[]^_{|}~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&()*+,-./:;<=>?@[]^_{|}~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123" >> $COMP_DIR/file-64k;
  : $(( i += 1 ))
done;

programs="examples/empty.c examples/hello.c examples/fib.c examples/cat.c examples/cp.c examples/wc.c examples/sha256sum.c"
runners="ksh dash bash yash zsh gcc pnut"

for prog in $programs; do
  for run_with in $runners; do
    compile "$run_with" $prog
  done
done

runners="ksh dash bash yash zsh gcc pnut"

for run_with in $runners; do
  compile "$run_with" "pnut.c" "-DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Dsh" "pnut-sh"
done

for run_with in $runners; do
  compile "$run_with" "pnut.c" "-DSUPPORT_INCLUDE -Di386" "pnut-exe"
done

programs="examples/empty.c examples/hello.c examples/fib.c examples/cat.c examples/cp.c examples/wc.c examples/sha256sum.c"
runners="ksh dash bash yash zsh gcc pnut"

run_program_options() { # $1 = program, $2 = runner
  case $1 in
    "examples/cat.c")       echo "$COMP_DIR/file-64k" ;;
    "examples/cp.c")        echo "$COMP_DIR/file-64k $COMP_DIR/file-64k-out-$2" ;;
    "examples/wc.c")        echo "$COMP_DIR/file-64k" ;;
    "examples/sha256sum.c") echo "$COMP_DIR/file-64k" ;;
    *)                      echo "" ;;
  esac
}

for prog in $programs; do
  for runner in $runners; do
    run "$runner" $prog $(run_program_options $prog $runner)
  done
done
