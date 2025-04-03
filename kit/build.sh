#! /bin/sh

# if pnut-exe; [ $? -eq 127 ]; then
#   echo "pnut-exe is not installed"
#   exit 1
# fi


# We need some file with the execute permission to bootstrap the chmod command.
# For now, we use the chmod command itself, but when distributing the archive,
# the chmod file will already be created (empty) with the execute permission.
touch kit/chmod; chmod +x kit/chmod

PNUT_CMD="./build/tcc/pnut-exe"
$PNUT_CMD -I portable_libc/include/ kit/chmod.c -o kit/chmod
if [ $? -ne 0 ]; then
  echo "Failed to compile chmod.c"
  exit 1
fi

compile_with_pnut() {
  $PNUT_CMD kit/$1.c -I portable_libc/include/ -o kit/$1
  if [ $? -ne 0 ]; then
    printf "Failed to compile %s.c" "$1"
    exit 1
  fi
  set -x
  ./kit/chmod 755 kit/$1
}

compile_with_pnut mkdir
compile_with_pnut untar
compile_with_pnut ungz

./kit/mkdir -p kit/build

# Change to build directory to avoid polluting the root directory even more
cd kit/build # shell built-in

./../ungz --file ../../../live-bootstrap/distfiles/124cfae4bfafec24dfea65117d0a407078beb459.tar.gz \
          --output 124cfae4bfafec24dfea65117d0a407078beb459.tar

./../untar 124cfae4bfafec24dfea65117d0a407078beb459.tar
