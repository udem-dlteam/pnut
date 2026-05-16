#! /bin/sh
#
# Extract dash-0.5.5 + deps (glibc, ncurses) from a Debian Squeeze image.
# Creates a .tar archive with the extracted files and an install script that can
# be used to copy them to the chroot environment.
#
# The original source is from Debian, and the hash matches the one from Debian:
# https://cdimage.debian.org/cdimage/archive/6.0.10/i386/iso-dvd/
#
URL="https://cdimage.debian.org/cdimage/archive/6.0.10/i386/iso-dvd/debian-6.0.10-i386-DVD-1.iso"
EXPECTED_HASH="59c77ffdd433e86abdc264b23e9294f147c07ff276cf7bdc61301562e1b8988b"

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

# For download_or_verify and mount_iso functions
. "$SCRIPT_DIR/utils.sh"

TEMP_DIR="$ROOT_DIR/build/scavenge/debian-6.0"
ISO_PATH="$TEMP_DIR/dvd-1.iso"
MOUNT_DIR="$TEMP_DIR/mount-1"

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

# Unpack all deb using (ar x) in the temp directory, then install them
mkdir -p $TEMP_DIR/dash $TEMP_DIR/glibc $TEMP_DIR/ncurses
ar x "$TEMP_DIR/dash_0.5.5.1-7.4_i386.deb"           --output "$TEMP_DIR/dash"
ar x "$TEMP_DIR/libc6_2.11.3-4_i386.deb"             --output "$TEMP_DIR/glibc"
ar x "$TEMP_DIR/libncurses5_5.7+20100313-5_i386.deb" --output "$TEMP_DIR/ncurses"

# Extract data.tar.gz from each deb and copy the relevant files to the chroot directory
tar -xf "$TEMP_DIR/dash/data.tar.gz" -C "$INSTALL_DIR"
tar -xf "$TEMP_DIR/glibc/data.tar.gz" -C "$INSTALL_DIR"
tar -xf "$TEMP_DIR/ncurses/data.tar.gz" -C "$INSTALL_DIR"
EOF

reproducible_tar -cf "$(dirname "$0")/dash-i386-squeeze.tar" \
  -C "$TEMP_DIR" "install.sh" \
  -C "$MOUNT_DIR/pool/main/d/dash" "dash_0.5.5.1-7.4_i386.deb" \
  -C "$MOUNT_DIR/pool/main/e/eglibc/" "libc6_2.11.3-4_i386.deb" \
  -C "$MOUNT_DIR/pool/main/n/ncurses" "libncurses5_5.7+20100313-5_i386.deb" \

# Unmount the ISO since we don't need it anymore
sudo umount "$MOUNT_DIR"
