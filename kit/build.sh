#! /bin/sh

set -e -u

# Parse the arguments
fast=0 # Use fast mode

while [ $# -gt 0 ]; do
  case $1 in
    --fast) fast=1;               shift 1;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

if [ -d kit/build ]; then
  rm -rf kit/build
fi

if [ ! -f "./build/tcc/pnut-exe" ]; then
  printf "Run utils/make-pnut-exe-for-tcc.sh first!\n"
  exit 1
fi

# We need some file with the execute permission to bootstrap the chmod command.
# For now, we use the chmod command itself, but when distributing the archive,
# the chmod file will already be created (empty) with the execute permission.
touch kit/chmod; chmod +x kit/chmod

PNUT_CMD="./build/tcc/pnut-exe"
$PNUT_CMD -I portable_libc/include/ kit/chmod.c portable_libc/libc.c -o kit/chmod
if [ $? -ne 0 ]; then
  printf "Failed to compile chmod.c\n"
  exit 1
fi

compile_with_pnut() {
  $PNUT_CMD kit/$1.c -I portable_libc/include/ portable_libc/libc.c -o kit/$1
  if [ $? -ne 0 ]; then
    printf "Failed to compile %s.c" "$1"
    exit 1
  fi
  ./kit/chmod 755 kit/$1
}

compile_with_pnut simple-patch
compile_with_pnut mkdir
compile_with_pnut untar
compile_with_pnut ungz

./kit/mkdir -p kit/build

# Change to build directory to avoid polluting the root directory even more
cd kit/build # shell built-in

if [ $fast -eq 0 ]; then
  ./../ungz --file ../tcc-0.9.27.tar.gz \
            --output tcc-0.9.27.tar

  ./../untar tcc-0.9.27.tar
else
  tar xf ../tcc-0.9.27.tar.gz
  if [ $? -ne 0 ]; then
    printf "Failed to extract tcc-0.9.27.tar.gz\n"
    exit 1
  fi
fi

cd tcc-0.9.27

# Apply patches to TCC's source code

patch_tcc() { # $1 = tcc file to patch, $2 = patch file
  # Check if the patch file exists
  if [ ! -f "$1" ]; then
    printf "TCC file %s not found!\n" "$1"
    exit 1
  fi

  if [ ! -f "../../tcc-patches/$2.before" ]; then
    printf "Patch file %s.before not found!\n" "$2"
    exit 1
  fi

  if [ ! -f "../../tcc-patches/$2.after" ]; then
    printf "Patch file %s.after not found!\n" "$2"
    exit 1
  fi

  printf "Applying patch %s to tcc/%s\n" "$2" "$1"

  ../../simple-patch "$1" \
                     "../../tcc-patches/$2.before" \
                     "../../tcc-patches/$2.after"
}

patches="
tccpp.c:array-sizeof
tcc.h:attribute
tcc.h:bitfields
libtcc.c:open-mode-arg-1
libtcc.c:open-mode-arg-2
libtcc.c:scanf_TCC_VERSION
tcc.h:undefine_TCC_IS_NATIVE
x86_64-gen.c:VLA
tccpp.c:tccpp-parse-integer
"
# elf.h:elf-typedefs
# elf.h:elf-auxv_t
# tcc.h:read-write64
# tcc.h:CValue
# tcc.h:ExprValue
# tccgen.c:switch_t
# tccasm.c:asm_expr_cmp

for patch in $patches; do
  file=${patch%%:*}
  patch_file=${patch#*:}
  if [ $file:$patch_file != $patch ]; then
    printf "Invalid patch format: %s\n" "$patch"
    exit 1
  fi

  patch_tcc "$file" "$patch_file"
done

# Create empty config.h file
printf "// Empty" > config.h

cd ../../.. # Back to root directory

printf "Compiling TCC...\n"

TCC_TARGET_ARCH=I386
PNUT_ARCH=i386_linux

$PNUT_CMD                                                   \
    -I portable_libc/include/                               \
    -D BOOTSTRAP=1                                          \
    -D HAVE_LONG_LONG=0                                     \
    -D TCC_TARGET_${TCC_TARGET_ARCH}=1                      \
    -D CONFIG_SYSROOT=\"/\"                                 \
    -D CONFIG_TCC_CRTPREFIX=\"boot0-lib\"                   \
    -D CONFIG_TCC_ELFINTERP=\"/mes/loader\"                 \
    -D CONFIG_TCC_SYSINCLUDEPATHS=\"include\"               \
    -D TCC_LIBGCC=\"boot0-lib/libc.a\"                      \
    -D CONFIG_TCC_LIBTCC1_MES=0                             \
    -D CONFIG_TCCBOOT=1                                     \
    -D CONFIG_TCC_STATIC=1                                  \
    -D CONFIG_USE_LIBGCC=1                                  \
    -D TCC_VERSION=\"0.9.26\"                               \
    -D ONE_SOURCE=1                                         \
    -D CONFIG_TCCDIR=\"boot0-lib/tcc\"                      \
    kit/build/tcc-0.9.27/tcc.c                              \
    portable_libc/libc.c                                    \
    -o kit/build/tcc-by-pnut                                \
    || { printf "Failed to compile TCC\n"; cat kit/build/tcc-by-pnut; exit 1; }

./kit/chmod 755 kit/build/tcc-by-pnut

./kit/build/tcc-by-pnut -vv
