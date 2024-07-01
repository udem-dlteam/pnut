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
  file="examples/$2.c"

  if [ $1 = "gcc" ]; then
    TIME_MS=$(( `bash -c "time $1 $COMP_DIR/pnut-sh-base.exe $file > $COMP_DIR/$2-with-$1.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  elif [ $1 = "pnut" ]; then
    TIME_MS=$(( `bash -c "time $1 $COMP_DIR/pnut-sh-compiled-by-pnut-exe.exe $file > $COMP_DIR/$2-with-$1.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  else
    TIME_MS=$(( `bash -c "time $1 $COMP_DIR/pnut.sh $file > $COMP_DIR/$2-with-$1.sh" 2>&1 | fgrep real | sed -e "s/real[^0-9]*//g" -e "s/m/*60000+/g" -e "s/s//g" -e "s/\\+0\\./-1000+1/g" -e "s/\\.//g"` ))
  fi
  print_time $TIME_MS "for: $1 with $file"
  # exit 1
}

PNUT_SH_OPTIONS="-DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Dsh"
PNUT_x86_OPTIONS="-DSUPPORT_INCLUDE -Di386"
gcc -o $COMP_DIR/pnut-sh-base.exe $PNUT_SH_OPTIONS -Os pnut.c
gcc -o $COMP_DIR/pnut-exe.exe $PNUT_x86_OPTIONS -Os pnut.c
# set -x
./$COMP_DIR/pnut-sh-base.exe $PNUT_SH_OPTIONS pnut.c > $COMP_DIR/pnut.sh
./$COMP_DIR/pnut-exe.exe $PNUT_SH_OPTIONS pnut.c > $COMP_DIR/pnut-sh-compiled-by-pnut-exe.exe

chmod +x $COMP_DIR/pnut-sh-compiled-by-pnut-exe.exe

programs="empty hello fib"
# programs="sha256sum"
shells="ksh dash bash yash zsh gcc pnut"

for prog in $programs; do
  for shell in $shells; do
    compile "$shell" $prog
  done
done
