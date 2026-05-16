#! /bin/sh
#
# Extract ksh_93q-2 + dependencies (glibc, ncurses) from a Debian Sarge image.
# Creates a .tar archive with the extracted files and an install script
# that can be used to copy them to the chroot environment.
#
# Debian no longer hosts the old ISOs, but we can find them on archive.org:
# https://archive.org/details/debian_3.1r8_i386
# The original source is from Debian, and the hash matches the one from Debian:
# https://cdimage.debian.org/cdimage/archive/3.1_r8/i386/iso-dvd/
# They can be recreated from scratch using the jigdo utility.
#
URL1="https://archive.org/download/debian-sarge-r8-32bit/debian-31r8-i386-binary-1-DVD.iso"
URL2="https://archive.org/download/debian-sarge-r8-32bit/debian-31r8-i386-binary-2-DVD.iso"
EXPECTED_HASH1="84b72a265ecdcf7ffb482703a2d7ab15c5f3d249fb372f41a66a9ec945cd4dd5"
EXPECTED_HASH2="f565ee79038847dbabc61cecc57528ac4735d127f2fc868280da1c321e88b629"

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

# For download_or_verify and mount_iso functions
. "$SCRIPT_DIR/utils.sh"

TEMP_DIR="$ROOT_DIR/build/scavenge/debian-3.1"
ISO1_PATH="$TEMP_DIR/dvd-1.iso"
ISO2_PATH="$TEMP_DIR/dvd-2.iso"
MOUNT1_DIR="$TEMP_DIR/mount-1"
MOUNT2_DIR="$TEMP_DIR/mount-2"

mkdir -p "$TEMP_DIR" "$MOUNT1_DIR" "$MOUNT2_DIR"

download_or_verify "$URL1" "$EXPECTED_HASH1" "$ISO1_PATH"
download_or_verify "$URL2" "$EXPECTED_HASH2" "$ISO2_PATH"

mount_iso "$ISO1_PATH" "$MOUNT1_DIR"
mount_iso "$ISO2_PATH" "$MOUNT2_DIR"

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
mkdir -p $TEMP_DIR/ksh $TEMP_DIR/glibc $TEMP_DIR/libcap $TEMP_DIR/ncurses
ar x "$TEMP_DIR/ksh_93q-2_i386.deb"               --output "$TEMP_DIR/ksh"
ar x "$TEMP_DIR/libc6_2.3.2.ds1-22sarge6_i386.deb"  --output "$TEMP_DIR/glibc"
ar x "$TEMP_DIR/libncurses5_5.4-4_i386.deb"         --output "$TEMP_DIR/ncurses"

# Extract data.tar.gz from each deb and copy the relevant files to the chroot directory
tar -xf "$TEMP_DIR/ksh/data.tar.gz" -C "$INSTALL_DIR"
tar -xf "$TEMP_DIR/glibc/data.tar.gz" -C "$INSTALL_DIR"
tar -xf "$TEMP_DIR/ncurses/data.tar.gz" -C "$INSTALL_DIR"

# Symlink ksh93 to ksh for convenience, since the original binary is named ksh93 in Debian 3.1
ln -s "/bin/ksh93" "$INSTALL_DIR/bin/ksh"
# Also create a symlink for sh pointing to ksh, since ksh can be used as a sh-compatible shell
ln -s "/bin/ksh" "$INSTALL_DIR/bin/sh"
EOF

reproducible_tar -cf "$(dirname "$0")/ksh-i386-sarge.tar" \
  -C "$TEMP_DIR" "install.sh" \
  -C "$MOUNT2_DIR/pool/main/k/ksh" "ksh_93q-2_i386.deb" \
  -C "$MOUNT1_DIR/pool/main/g/glibc/" "libc6_2.3.2.ds1-22sarge6_i386.deb" \
  -C "$MOUNT1_DIR/pool/main/n/ncurses" "libncurses5_5.4-4_i386.deb" \

# Unmount the ISO since we don't need it anymore
sudo umount "$MOUNT1_DIR"
sudo umount "$MOUNT2_DIR"
