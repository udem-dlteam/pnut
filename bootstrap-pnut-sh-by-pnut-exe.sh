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
PNUT_SH_OPTIONS="$PNUT_OPTIONS -Dsh"

# Parse the arguments
backend="x86_64_linux"  # Default to x86_64_linux
minimal_pnut=0 # Enable PNUT_BOOTSTRAP for bootstrapping

while [ $# -gt 0 ]; do
  case $1 in
    --backend)      backend="$2";            shift 2 ;;
    --minimal-pnut) minimal_pnut=1;          shift 1 ;;
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

if [ $minimal_pnut -eq 1 ]; then
  PNUT_EXE_OPTIONS="$PNUT_EXE_OPTIONS -DPNUT_BOOTSTRAP"
  PNUT_SH_OPTIONS="$PNUT_SH_OPTIONS -DPNUT_BOOTSTRAP"
fi

# Compile pnut with gcc
gcc -o $TEMP_DIR/pnut-exe-by-gcc.exe $PNUT_EXE_OPTIONS pnut.c
gcc -o $TEMP_DIR/pnut-sh-by-gcc.exe $PNUT_SH_OPTIONS pnut.c

# Generate pnut-sh.sh with pnut-sh-by-gcc.exe. This is the reference pnut-sh.sh
./$TEMP_DIR/pnut-sh-by-gcc.exe $PNUT_SH_OPTIONS pnut.c > "$TEMP_DIR/pnut-sh.sh"

# Compile pnut-exe with pnut-exe compiled by gcc
./$TEMP_DIR/pnut-exe-by-gcc.exe $PNUT_EXE_OPTIONS pnut.c > "$TEMP_DIR/pnut-exe-by-pnut-exe.exe"

chmod +x $TEMP_DIR/pnut-exe-by-pnut-exe.exe

# Compile pnut-sh with pnut-exe compiled by gcc
./$TEMP_DIR/pnut-exe-by-pnut-exe.exe $PNUT_SH_OPTIONS pnut.c > "$TEMP_DIR/pnut-sh-by-pnut-exe.exe"

chmod +x $TEMP_DIR/pnut-sh-by-pnut-exe.exe

./$TEMP_DIR/pnut-sh-by-pnut-exe.exe $PNUT_SH_OPTIONS pnut.c > "$TEMP_DIR/pnut-sh-by-pnut-sh-by-pnut-exe.sh"

diff -q $TEMP_DIR/pnut-sh.sh $TEMP_DIR/pnut-sh-by-pnut-sh-by-pnut-exe.sh || {
  echo "pnut-sh.sh != pnut-sh-by-pnut-sh-by-pnut-exe.sh"
  exit 1
}

echo "Success!"
sha256sum $TEMP_DIR/pnut-sh.sh $TEMP_DIR/pnut-sh-by-pnut-sh-by-pnut-exe.sh
