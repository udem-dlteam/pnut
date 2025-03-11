#! /bin/sh
#
# Usage: decode-machine-code.sh <binary-file>

set -e

if [ $# -ne 1 ]; then
    echo "Usage: decode-machine-code.sh <binary-file>"
    exit 1
fi

start_address_i386_elf="84"
start_address_x86_64_elf="120"

i386_arch="i386"
x86_64_arch="i386:x86-64"

elf_type=$(readelf -h "$1" | grep "Class:" | awk '{print $2}')
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
# opts="--no-show-raw-insn --no-addresses"

echo "$1: $elf_type"
objdump -b binary -M intel --start-address="$start_address" -m "$architecture" -D "$1"
