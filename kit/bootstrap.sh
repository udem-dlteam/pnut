#! /bin/sh
#
# Script to bootstrap tcc after jammed.sh was extracted.
# It prepares the environment and builds the necessary tools, before building TCC.

set -e -u -x

: ${PNUT_OPTIONS:=} # Default to empty options
: ${INSTALL_EXECS:=1} # Default to installing

# 1. Unpack jammed.sh
# ./jammed.sh

# 2. Copy jammed.sh if jammed-no-exec.sh doesn't exist
if [ ! -e "jammed-no-exec.sh" ]; then
  $SHELL cat.sh jammed.sh > jammed-no-exec.sh
fi

PNUT_EXE_OPTIONS="$PNUT_OPTIONS -Dtarget_i386_linux -DONE_PASS_GENERATOR"

# 3. Bootstrap pnut-exe (if necessary)
if [ ! -e "pnut-exe" ]; then
  # 3a. Bootstrap pnut-exe
  printf "Bootstrapping pnut-exe from pnut-sh.sh\n"
  $SHELL pnut-sh.sh pnut.c $PNUT_EXE_OPTIONS > pnut-exe.sh
  # 3b. Make executable version of pnut-exe. Overwrite jammed.sh to reuse its execute bit.
  printf "Making executable pnut-exe\n"
  $SHELL pnut-exe.sh pnut.c $PNUT_EXE_OPTIONS -DBOOTSTRAP_TCC -o jammed.sh

  # 4. Compile bintools with pnut-exe (named jammed.sh)
  printf "Compiling bintools with bootstrapped pnut-exe\n"
  ./jammed.sh -D FLAT_INCLUDES -I "./" bintools.c bintools-libc.c -o bintools
  # 4b. Make executable version of bintools. Overwrite jammed.sh to reuse its execute bit.
  ./bintools cp jammed.sh pnut-exe
  ./bintools chmod 755 pnut-exe
else
  printf "pnut-exe already exists, skipping pnut-exe bootstrap\n"
  # 4. Compile bintools with pnut-exe
  ./pnut-exe -D FLAT_INCLUDES -I "./" bintools.c bintools-libc.c -o bintools
fi

# 5. Install bintools and pnut-exe
if [ $INSTALL_EXECS -eq 1 ]; then
  printf "Installing bintools and pnut-exe\n"
  ./bintools mkdir -p /usr/bin
  for tool in cp chmod mkdir sha256sum simple-patch ungz untar; do
    ./bintools cp ./bintools /usr/bin/$tool
    ./bintools chmod 755 /usr/bin/$tool
  done
  ./bintools cp ./pnut-exe /usr/bin/pnut-exe
  ./bintools chmod 755 /usr/bin/pnut-exe
else
  printf "Skipping installation of bintools and pnut-exe as requested\n"
fi

# 6. Extract the rest of the files (now that mkdir is available)
$SHELL ./jammed-no-exec.sh --force-no-exec
