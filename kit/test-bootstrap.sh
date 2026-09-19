#! /bin/sh
#
# test-bootstrap.sh: Test that the TCC bootstrap process is reproducible.

# The following options are passed directly to make-jammed.sh and control which
# files are included in the jammed.sh archive seed:
#   --include-utils: Seed the environment with debugging scripts.
#   --extract-archives: Extract .tar.gz files instead of passing them to jam.sh
#   --mes-libc: Use mes libc instead of pnut libc (default: pnut libc)
#   --tcc-version: Version of tcc to use (default: 0.9.27)

set -eu

TEMP_DIR="build"
mkdir -p "$TEMP_DIR"

ROOTFS_DIR_PNUT="$TEMP_DIR/rootfs-pnut"
ROOTFS_DIR_GCC="$TEMP_DIR/rootfs-gcc"

checksum_result() {
  # Normalize tags by stripping the path prefix from the checksum output.
  # Not quite how tags are meant to be used, but whatever.
  sha256sum --tag \
    $1/build/tcc-boot2.o $1/build/tcc-boot3.o \
    $1/build/tcc-boot2   $1/build/tcc-boot3 \
    | sed "s|$1/||g"
}

# Bootstrap in empty chroot environment
sudo rm -rf $ROOTFS_DIR_PNUT && ./kit/setup-rootfs.sh --dir $ROOTFS_DIR_PNUT \
  --bootstrap-shell ksh-i386-sarge \
  --include-utils \
  --extract-archives \
  --skip-shell-bootstrap \
  "$@"

sudo chroot $ROOTFS_DIR_PNUT /bin/sh -c "sh jammed.sh && INSTALL_EXECS=1 BOOTSTRAP_SHELL=/bin/sh USE_GCC=0 sh bootstrap.sh"

checksum_result $ROOTFS_DIR_PNUT > "$TEMP_DIR/checksums-pnut"

sudo rm -rf $ROOTFS_DIR_GCC && ./kit/setup-rootfs.sh --dir $ROOTFS_DIR_GCC \
  --bootstrap-shell ksh-i386-sarge \
  --include-utils \
  --extract-archives \
  --skip-shell-bootstrap \
  "$@"

# This uses gcc to compile tcc, so we must run the bootstrap script outside of
# the chroot since gcc isn't available in the chroot environment.
sh -c "cd $ROOTFS_DIR_GCC && sh jammed.sh && INSTALL_EXECS=0 BOOTSTRAP_SHELL=/bin/sh USE_GCC=1 sh bootstrap.sh"

checksum_result $ROOTFS_DIR_GCC > "$TEMP_DIR/checksums-gcc"

{ diff "$TEMP_DIR/checksums-pnut" "$TEMP_DIR/checksums-gcc" && echo "Checksums match, bootstrap is reproducible."; } \
  || { echo "Checksums do not match, bootstrap is not reproducible."; exit 1; }
