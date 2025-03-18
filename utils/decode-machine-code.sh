#! /bin/sh
#
# Usage: decode-machine-code.sh <binary-file> --pnut

set -e

if [ $# -ne 1 ]; then
    echo "Usage: decode-machine-code.sh <binary-file>"
    exit 1
fi

file="$1" shift
pnut_exe=0
for_diff=0 # Remove addresses and raw instructions so diff can work

while [ $# -gt 0 ]; do
  case $1 in
    --pnut-exe) pnut_exe="1";     shift 1 ;;
    --for-diff) for_diff="1";     shift 1 ;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

start_address_i386_elf="84"
start_address_x86_64_elf="120"

i386_arch="i386"
x86_64_arch="i386:x86-64"

elf_type=$(readelf -h "$file" | grep "Class:" | awk '{print $2}')
if [ "$elf_type" = "ELF32" ]; then
    start_address="$start_address_i386_elf"
    architecture="$i386_arch"
elif [ "$elf_type" = "ELF64" ]; then
    start_address="$start_address_x86_64_elf"
    architecture="$x86_64_arch"
else
    echo "Error: Unsupported architecture"
    exit 1
fi

# Options useful when comparing 2 objdump outputs
if [ $for_diff -eq 1 ]; then
    objdump_opts="--no-show-raw-insn --no-addresses"
else
    objdump_opts=""
fi

echo "$file: $elf_type"
if [ $pnut_exe -eq 1 ]; then
    objdump $objdump_opts -b binary -M intel --start-address="$start_address" -m "$architecture" -D "$file"
else
    objdump $objdump_opts -b binary -M intel -m "$architecture" -D "$file"
fi
