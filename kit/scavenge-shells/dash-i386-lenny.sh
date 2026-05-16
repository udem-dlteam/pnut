#! /bin/sh
#
# Extract dash-0.5.4-12 + deps (glibc, ncurses) from a Debian Lenny image.
# Creates a .tar archive with the extracted files and an install script that can
# be used to copy them to the chroot environment.
#
# The original source is from Debian, and the hash matches the one from Debian:
# https://cdimage.debian.org/cdimage/archive/5.0.10/i386/iso-dvd/
#
URL="https://cdimage.debian.org/cdimage/archive/5.0.10/i386/iso-dvd/debian-5010-i386-DVD-1.iso"
EXPECTED_HASH="0a21a8e09c0e1da93f01cee58c6a5793fd9399bdb75176141cfb4f6861d093c2"

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

# For download_or_verify and mount_iso functions
. "$SCRIPT_DIR/utils.sh"

TEMP_DIR="$ROOT_DIR/build/scavenge/debian-5.0"
ISO_PATH="$TEMP_DIR/debian-5010-i386-DVD-1.iso"
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
ar x "$TEMP_DIR/dash_0.5.4-12_i386.deb"              --output "$TEMP_DIR/dash"
ar x "$TEMP_DIR/libc6_2.7-18lenny7_i386.deb"         --output "$TEMP_DIR/glibc"
ar x "$TEMP_DIR/libncurses5_5.7+20081213-1_i386.deb" --output "$TEMP_DIR/ncurses"

# Extract data.tar.gz from each deb and copy the relevant files to the chroot directory
tar -xf "$TEMP_DIR/dash/data.tar.gz" -C "$INSTALL_DIR"
tar -xf "$TEMP_DIR/glibc/data.tar.gz" -C "$INSTALL_DIR"
tar -xf "$TEMP_DIR/ncurses/data.tar.gz" -C "$INSTALL_DIR"

# Also create a symlink for sh pointing to dash, since dash can be used as a sh-compatible shell
ln -s "/bin/dash" "$INSTALL_DIR/bin/sh"
EOF

reproducible_tar -cf "$(dirname "$0")/dash-i386-lenny.tar" \
  -C "$TEMP_DIR" "install.sh" \
  -C "$MOUNT_DIR/pool/main/d/dash" "dash_0.5.4-12_i386.deb" \
  -C "$MOUNT_DIR/pool/main/g/glibc/" "libc6_2.7-18lenny7_i386.deb" \
  -C "$MOUNT_DIR/pool/main/n/ncurses" "libncurses5_5.7+20081213-1_i386.deb" \

# Unmount the ISO since we don't need it anymore
sudo umount "$MOUNT_DIR"
