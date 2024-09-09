#! /bin/sh
#
# Check that pnut-exe can compile pnut-sh.
# This is not a something we'll usually want in a normal boostrap process, but
# it serves as a good test of pnut-exe. Note that it doesn't use any shell as
# all the commands invoke binary versions of pnut.

set -e -u

TEMP_DIR="build"

mkdir -p $TEMP_DIR

: ${PNUT_OPTIONS:=} # Default to empty options

PNUT_EXE_OPTIONS="$PNUT_OPTIONS" # Backend is set by the backend option
PNUT_SH_OPTIONS="$PNUT_OPTIONS -DRT_NO_INIT_GLOBALS -Dsh"

# Parse the arguments
backend="x86_64_linux"  # Default to x86_64_linux

while [ $# -gt 0 ]; do
  case $1 in
    --backend) backend="$2";                            shift 2 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

case $backend in
  x86_64_mac | x86_64_linux | i386_linux)
    PNUT_EXE_OPTIONS="$PNUT_EXE_OPTIONS -Dtarget_$backend" ;;
  *)
    echo "Unknown backend: $backend"
    echo "Supported backends: x86_64_mac x86_64_linux i386_linux"
    exit 1
    ;;
esac

# Increase the stack because pnut-sh's global variables are initialized on the stack
ulimit -s 16384
stack_limit=$(ulimit -s)
if [ "$stack_limit" = "unlimited" ] || [ "$stack_limit" -lt 16384 ]; then
  echo "Stack size is too small: $stack_limit"
  exit 1
fi

# Compile pnut with gcc
gcc -o $TEMP_DIR/pnut-exe-by-gcc.exe $PNUT_EXE_OPTIONS pnut.c
gcc -o $TEMP_DIR/pnut-sh-by-gcc.exe $PNUT_SH_OPTIONS pnut.c

# Generate pnut.sh with pnut-sh-by-gcc.exe. This is the reference pnut.sh
./$TEMP_DIR/pnut-sh-by-gcc.exe $PNUT_SH_OPTIONS pnut.c > "$TEMP_DIR/pnut.sh"

# Compile pnut-exe with pnut-exe compiled by gcc
./$TEMP_DIR/pnut-exe-by-gcc.exe $PNUT_EXE_OPTIONS pnut.c > "$TEMP_DIR/pnut-exe-by-pnut-exe.exe"

chmod +x $TEMP_DIR/pnut-exe-by-pnut-exe.exe

# Compile pnut-sh with pnut-exe compiled by gcc
./$TEMP_DIR/pnut-exe-by-pnut-exe.exe $PNUT_SH_OPTIONS pnut.c > "$TEMP_DIR/pnut-sh-by-pnut-exe.exe"

chmod +x $TEMP_DIR/pnut-sh-by-pnut-exe.exe

./$TEMP_DIR/pnut-sh-by-pnut-exe.exe $PNUT_SH_OPTIONS pnut.c > "$TEMP_DIR/pnut-sh-by-pnut-sh-by-pnut-exe.sh"

diff -q $TEMP_DIR/pnut.sh $TEMP_DIR/pnut-sh-by-pnut-sh-by-pnut-exe.sh || {
  echo "pnut.sh != pnut-sh-by-pnut-sh-by-pnut-exe.sh"
  exit 1
}

echo "Success!"
sha256sum $TEMP_DIR/pnut.sh $TEMP_DIR/pnut-sh-by-pnut-sh-by-pnut-exe.sh
