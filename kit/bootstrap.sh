#! /bin/sh
#
# Script to bootstrap tcc after jammed.sh was extracted.
# It prepares the environment and builds the necessary tools, before building TCC.

set -e -u -x

# 1. Unpack jammed.sh
# ./jammed.sh

# 2. Copy jammed.sh
$SHELL cat.sh jammed.sh > jammed-no-exec.sh

# 3. Bootstrap pnut-exe (if necessary)
if [ ! -e "pnut-exe" ]; then
  # 3a. Bootstrap pnut-exe
  $SHELL pnut-sh.sh pnut.c -Dtarget_i386_linux -DONE_PASS_GENERATOR > pnut-exe.sh
  # 3b. Make executable version of pnut-exe. Overwrite jammed.sh to reuse its execute bit.
  $SHELL pnut-exe.sh pnut.c -Dtarget_i386_linux -DBOOTSTRAP_TCC -DONE_PASS_GENERATOR -o jammed.sh

  # 4. Compile bintools with pnut-exe (named jammed.sh)
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
for tool in cp chmod mkdir sha256sum simple-patch ungz untar; do
  ./bintools cp ./bintools /usr/bin/$tool
  ./bintools chmod 755 /usr/bin/$tool
done
cp ./pnut-exe /usr/bin/pnut-exe
chmod 755 /usr/bin/pnut-exe

# 6. Extract the rest of the files (now that mkdir is available)
$SHELL ./jammed-no-exec.sh --force-no-exec

# 7. Unpack tcc if in .tar.gz
if [ -e  kit/tcc-0.9.26.tar.gz ]; then
  ungz --file kit/tcc-0.9.26.tar.gz --output tcc-0.9.26.tar
  untar tcc-0.9.26.tar
elif [ ! -e "tcc-0.9.26-1147-gee75a10c" ]; then
  printf "Error: tcc-0.9.26-1147-gee75a10c not found\n"
  exit 1
fi

# 8. Bootstrap tcc

set +x # Disable debugging

TEMP_DIR="build"
TCC_DIR="tcc-0.9.26-1147-gee75a10c"
INCLUDE_PATH="portable_libc/include" # pnut libc
PATCHES_DIR="kit/tcc-patches"

TCC_TARGET_ARCH=I386
PNUT_ARCH=i386_linux

: ${PNUT_OPTIONS:=} # Default to empty options

PNUT_EXE_OPTIONS="$PNUT_OPTIONS -DBOOTSTRAP_LONG -Dtarget_$PNUT_ARCH -DUNDEFINED_LABELS_ARE_RUNTIME_ERRORS -DENABLE_PNUT_INLINE_INTERRUPT -DONE_PASS_GENERATOR"

if [ ! -d "$TEMP_DIR" ]; then mkdir -p "$TEMP_DIR"; fi

# Make a version of pnut-exe that doesn't include the built-in libc
# pnut-exe $PNUT_EXE_OPTIONS -DNO_BUILTIN_LIBC pnut.c > $TEMP_DIR/pnut-exe-for-tcc
# chmod +x $TEMP_DIR/pnut-exe-for-tcc
# sha256sum $TEMP_DIR/pnut-exe-for-tcc

# Step 0: Patch TCC

pcat() {
  IFS=
  while read -r line; do
    printf "%s\n" "$line"
  done
}

cp kit/config.h $TCC_DIR/config.h

# simple-patch ${TCC_DIR}/tcctools.c  $PATCHES_DIR/remove-fileopen.before $PATCHES_DIR/remove-fileopen.after \
#   || { printf "Failed to patch tcctools.c with remove-fileopen\n"; exit 1; }
# simple-patch ${TCC_DIR}/tcctools.c  $PATCHES_DIR/addback-fileopen.before $PATCHES_DIR/addback-fileopen.after \
#   || { printf "Failed to patch tcctools.c with addback-fileopen\n"; exit 1; }
simple-patch ${TCC_DIR}/tccpp.c     $PATCHES_DIR/array_sizeof.before $PATCHES_DIR/array_sizeof.after \
  || { printf "Failed to patch tccpp.c with array_sizeof\n"; exit 1; }
# setjump always returns 0, not needed
# simple-patch ${TCC_DIR}/libtcc.c    $PATCHES_DIR/error_set_jmp_enabled.before $PATCHES_DIR/error_set_jmp_enabled.after \
#   || { printf "Failed to patch libtcc.c with error_set_jmp_enabled\n"; exit 1; }
simple-patch ${TCC_DIR}/tccgen.c    $PATCHES_DIR/fix_stack_64_bit_operands_on_32_bit.before $PATCHES_DIR/fix_stack_64_bit_operands_on_32_bit.after \
  || { printf "Failed to patch tccgen.c with fix_stack_64_bit_operands_on_32_bit\n"; exit 1; }
simple-patch ${TCC_DIR}/tccgen.c       $PATCHES_DIR/float_negation.before $PATCHES_DIR/float_negation.after \
  || { printf "Failed to patch tccgen.c with float_negation\n"; exit 1; }
simple-patch ${TCC_DIR}/libtcc.c    $PATCHES_DIR/sscanf_TCC_VERSION.before $PATCHES_DIR/sscanf_TCC_VERSION.after \
  || { printf "Failed to patch libtcc.c with sscanf_TCC_VERSION\n"; exit 1; }
simple-patch ${TCC_DIR}/tcc.h       $PATCHES_DIR/undefine_TCC_IS_NATIVE.before $PATCHES_DIR/undefine_TCC_IS_NATIVE.after \
  || { printf "Failed to patch tcc.h with undefine_TCC_IS_NATIVE\n"; exit 1; }

# Step 1: Bootstrap initial version of TCC (tcc-pnut)

# Compile tcc-pnut
pnut-exe                                                                     \
  -I portable_libc/include/                                                  \
  -D BOOTSTRAP=1                                                             \
  -D HAVE_LONG_LONG=0                                                        \
  -D TCC_TARGET_${TCC_TARGET_ARCH}=1                                         \
  -D CONFIG_SYSROOT=\"/\"                                                    \
  -D CONFIG_TCC_CRTPREFIX=\"$TEMP_DIR/boot0-lib\"                            \
  -D CONFIG_TCC_ELFINTERP=\"/mes/loader\"                                    \
  -D CONFIG_TCC_SYSINCLUDEPATHS=\"$INCLUDE_PATH\"                            \
  -D TCC_LIBGCC=\"$TEMP_DIR/boot0-lib/libc.a\"                               \
  -D CONFIG_TCC_LIBTCC1_MES=0                                                \
  -D CONFIG_TCCBOOT=1                                                        \
  -D CONFIG_TCC_STATIC=1                                                     \
  -D CONFIG_USE_LIBGCC=1                                                     \
  -D TCC_VERSION=\"0.9.26\"                                                  \
  -D ONE_SOURCE=1                                                            \
  -D CONFIG_TCCDIR=\"$TEMP_DIR/boot0-lib/tcc\"                               \
  -D __intptr_t_defined=1                                                    \
  $TCC_DIR/tcc.c                                                             \
  portable_libc/libc.c                                                       \
  -o $TEMP_DIR/tcc-pnut

chmod 755 $TEMP_DIR/tcc-pnut

go() { # $1: name of bootstrap comp, $2: name of new compiler, $3: lib path (= $2 if empty)
  CC="$1"
  NEW_CC="$2" # Suffix of new compiler: "boot0", "boot1", "boot2", ...
  if [ $# -lt 3 ]; then
    LIB_PATH="$TEMP_DIR/$NEW_CC-lib"
  else
    LIB_PATH="$TEMP_DIR/$3-lib"
  fi

  mkdir -p "$LIB_PATH"
  mkdir -p "$LIB_PATH/tcc"

  $CC -c portable_libc/src/crt1.c -o "$LIB_PATH/crt1.o"
  printf "" > "$LIB_PATH/crtn.o" # Empty file
  printf "" > "$LIB_PATH/crti.o" # Empty file

  $CC -c -D ADD_LIBC_STUB -I portable_libc/include -o "$LIB_PATH/libc.o" portable_libc/libc.c
  $CC -ar cr "$LIB_PATH/libc.a" "$LIB_PATH/libc.o"

  # libtcc1.a
  $CC -c -o "$LIB_PATH/libtcc1.o" kit/libtcc1.c
  $CC -ar cr "$LIB_PATH/tcc/libtcc1.a" "$LIB_PATH/libtcc1.o"

  # We can now compile tcc-$NEW_CC
  $CC \
      -g \
      -v \
      -static \
      -o $TEMP_DIR/tcc-$NEW_CC \
      -D BOOTSTRAP=1 \
      -D HAVE_FLOAT=1 \
      -D HAVE_BITFIELD=1 \
      -D HAVE_LONG_LONG=1 \
      -D HAVE_SETJMP=1 \
      -I $INCLUDE_PATH \
      -D TCC_TARGET_${TCC_TARGET_ARCH}=1 \
      -D CONFIG_TCCDIR=\"$LIB_PATH/tcc\" \
      -D CONFIG_TCC_CRTPREFIX=\"$LIB_PATH\" \
      -D CONFIG_TCC_LIBPATHS=\"$LIB_PATH:$LIB_PATH/tcc\" \
      -D CONFIG_TCC_SYSINCLUDEPATHS=\"$INCLUDE_PATH\" \
      -D TCC_LIBGCC=\"$LIB_PATH/libc.a\" \
      -D TCC_LIBTCC1=\"libtcc1.a\" \
      -D CONFIG_TCC_ELFINTERP=\"/mes/loader\" \
      -D CONFIG_TCCBOOT=1 \
      -D CONFIG_TCC_STATIC=1 \
      -D CONFIG_USE_LIBGCC=1 \
      -D TCC_VERSION=\"0.9.26\" \
      -D ONE_SOURCE=1 \
      -L $LIB_PATH \
      $TCC_DIR/tcc.c

  sha256sum $LIB_PATH/crt1.o $LIB_PATH/crtn.o $LIB_PATH/crti.o
}

go "$TEMP_DIR/tcc-pnut" "boot0"
go "$TEMP_DIR/tcc-boot0" "boot1"
go "$TEMP_DIR/tcc-boot1" "boot2"
# Make sure we've reached a fixed point
go "$TEMP_DIR/tcc-boot2" "boot3" "boot2"

# Confirm with hashes that we're at a fixed point
sha256sum $TEMP_DIR/boot0-lib/crt1.o $TEMP_DIR/boot1-lib/crt1.o $TEMP_DIR/boot2-lib/crt1.o
sha256sum $TEMP_DIR/boot0-lib/tcc/libtcc1.a $TEMP_DIR/boot0-lib/libc.a
sha256sum $TEMP_DIR/boot1-lib/tcc/libtcc1.a $TEMP_DIR/boot1-lib/libc.a
sha256sum $TEMP_DIR/boot2-lib/tcc/libtcc1.a $TEMP_DIR/boot2-lib/libc.a
sha256sum $TEMP_DIR/tcc-boot0 $TEMP_DIR/tcc-boot1 $TEMP_DIR/tcc-boot2 $TEMP_DIR/tcc-boot3
