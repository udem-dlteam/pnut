#! /bin/sh
#
# Extract bash-2.05a and its dependencies (glibc, ncurses) from a Debian Woody
# ISO. Creates a .tar archive with the extracted files that can be used to copy
# them to the chroot environment.

URL="https://cdimage.debian.org/cdimage/archive/3.0_r6/i386/iso-cd/debian-30r6-i386-binary-1.iso"
# Published hash is MD5, which is known to be weak so we use SHA256 instead.
EXPECTED_HASH="c1df8c0f65af8ac97db0dd449d8a82e3259202ec36e7233caf894c66f679ff0e"

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

# For download_or_verify and mount_iso functions
. "$SCRIPT_DIR/utils.sh"

TEMP_DIR="$ROOT_DIR/build/scavenge/debian-3.0"
ISO_PATH="$TEMP_DIR/debian.iso"
MOUNT_DIR="$TEMP_DIR/mount"

mkdir -p "$TEMP_DIR" "$MOUNT_DIR"

download_or_verify "$URL" "$EXPECTED_HASH" "$ISO_PATH"

mount_iso "$ISO_PATH" "$MOUNT_DIR"

# Create install script
cat > "$TEMP_DIR/install.sh" <<'EOF'
#!/bin/sh

set -e

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <temp_dir> <install_dir>"
  exit 1
fi

TEMP_DIR="$1"
INSTALL_DIR="$2"

# Unpack all deb using (ar x) in the temp directory, then extract the
# data.tar.gz from each deb to the chroot directory.
mkdir -p $TEMP_DIR/bash_2.05a-11_i386
ar x "$TEMP_DIR/bash_2.05a-11_i386.deb"               --output "$TEMP_DIR/bash_2.05a-11_i386"
tar -xf "$TEMP_DIR/bash_2.05a-11_i386/data.tar.gz"    -C "$INSTALL_DIR"

mkdir -p $TEMP_DIR/libc6_2.2.5-11.8_i386
ar x "$TEMP_DIR/libc6_2.2.5-11.8_i386.deb"            --output "$TEMP_DIR/libc6_2.2.5-11.8_i386"
tar -xf "$TEMP_DIR/libc6_2.2.5-11.8_i386/data.tar.gz" -C "$INSTALL_DIR"

mkdir -p $TEMP_DIR/libncurses5_5.2.20020112a-7_i386
ar x "$TEMP_DIR/libncurses5_5.2.20020112a-7_i386.deb"            --output "$TEMP_DIR/libncurses5_5.2.20020112a-7_i386"
tar -xf "$TEMP_DIR/libncurses5_5.2.20020112a-7_i386/data.tar.gz" -C "$INSTALL_DIR"

EOF

# Create .tar containing the install script and the deb files
reproducible_tar -cf "$SCRIPT_DIR/bash-i386-woody.tar" \
  -C "$TEMP_DIR" "install.sh" \
  -C "$MOUNT_DIR/pool/main/b/bash" "bash_2.05a-11_i386.deb" \
  -C "$MOUNT_DIR/pool/main/g/glibc" "libc6_2.2.5-11.8_i386.deb" \
  -C "$MOUNT_DIR/pool/main/n/ncurses" "libncurses5_5.2.20020112a-7_i386.deb"

# Unmount the ISO since we don't need it anymore
sudo umount "$MOUNT_DIR"
