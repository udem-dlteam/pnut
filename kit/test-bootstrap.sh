#! /bin/sh
#
# test-bootstrap.sh: Test that the TCC bootstrap process is reproducible.

# The following options are passed directly to make-jammed.sh and control which
# files are included in the jammed.sh archive seed:
#   --include-utils: Seed the environment with debugging scripts.
#   --extract-archives: Extract .tar.gz files instead of passing them to jam.sh
#   --mes-libc: Use mes libc instead of pnut libc (default: pnut libc)
#   --tcc-version: Version of tcc to use (default: 0.9.27)
#
# The architecture of the chain can be selected as well (see --arch in
# kit/setup-rootfs.sh):
#   --arch <i386|x86_64>: Architecture of the bootstrapped chain (default:
#                         i386). The bootstrap shell stays the 32-bit scavenged
#                         ksh in both cases: pnut-sh.sh is a compiler written in
#                         shell script, so it still emits 64-bit code.
#
# The following option controls how the first pnut-exe of the environment that is
# compared against the gcc bootstrap is obtained (default: compiled by the host C
# compiler, which is taken from CC, for example CC=gcc-16):
#   --bootstrap-from-c4: Compile it with c4, from the kit/bootstrap-C4 submodule

set -eu

error() {
  printf "Error: %s\n" "$1" >&2
  exit 1
}

TEMP_DIR="build"
mkdir -p "$TEMP_DIR"

# Architecture of the chain under test. It is forwarded to kit/setup-rootfs.sh,
# which forwards it to the bootstrap scripts as BOOTSTRAP_ARCH, and it also
# decides the names of the chroot environments and checksum files, so that the
# artifacts of an i386 and of an x86-64 run can be kept side by side.
ARCH="i386"

# kit/setup-rootfs.sh and kit/bootstrap.sh both need to know which seed to use.
BOOTSTRAP_FROM="shell"
SETUP_OPTS=""
while [ $# -gt 0 ]; do
  case "$1" in
    --bootstrap-from-c4) BOOTSTRAP_FROM="c4"; shift 1 ;;
    --arch)
      if [ $# -lt 2 ]; then error "Missing argument for --arch option."; fi
      case "$2" in
        i386|x86_64) ARCH="$2" ;;
        *) error "Unknown architecture: $2 (expected i386 or x86_64)." ;;
      esac
      shift 2
      ;;
    *) SETUP_OPTS="$SETUP_OPTS $1"; shift 1 ;;
  esac
done

if [ "$ARCH" = i386 ]; then
  ARCH_SUFFIX=""   # Keep the historical names for the default architecture.
else
  ARCH_SUFFIX="-$ARCH"
fi

ROOTFS_DIR_PNUT="$TEMP_DIR/rootfs-pnut$ARCH_SUFFIX"
ROOTFS_DIR_GCC="$TEMP_DIR/rootfs-gcc$ARCH_SUFFIX"

# The gcc bootstrap always starts from a pnut-exe compiled by the host C
# compiler; the pnut one can start from c4 instead.
if [ "$BOOTSTRAP_FROM" = c4 ]; then
  PNUT_SEED_OPTS="--bootstrap-from-c4"
else
  PNUT_SEED_OPTS="--skip-initial-bootstrap"
fi

checksum_result() {
  # Normalize tags by stripping the path prefix from the checksum output.
  # Not quite how tags are meant to be used, but whatever.
  sha256sum --tag \
    $1/build/tcc-boot2.o $1/build/tcc-boot3.o \
    $1/build/tcc-boot2   $1/build/tcc-boot3 \
    | sed "s|$1/||g"
}

echo "Testing the reproducibility of the $ARCH bootstrap"

# Bootstrap in empty chroot environment
sudo rm -rf $ROOTFS_DIR_PNUT && ./kit/setup-rootfs.sh --dir $ROOTFS_DIR_PNUT \
  --bootstrap-shell ksh-i386-sarge \
  --arch "$ARCH" \
  --include-utils \
  --extract-archives \
  $PNUT_SEED_OPTS \
  $SETUP_OPTS

sudo chroot $ROOTFS_DIR_PNUT /bin/sh -c "sh jammed.sh && INSTALL_EXECS=1 BOOTSTRAP_SHELL=/bin/sh BOOTSTRAP_FROM=$BOOTSTRAP_FROM BOOTSTRAP_ARCH=$ARCH USE_GCC=0 sh bootstrap.sh"

checksum_result $ROOTFS_DIR_PNUT > "$TEMP_DIR/checksums-pnut$ARCH_SUFFIX"

sudo rm -rf $ROOTFS_DIR_GCC && ./kit/setup-rootfs.sh --dir $ROOTFS_DIR_GCC \
  --bootstrap-shell ksh-i386-sarge \
  --arch "$ARCH" \
  --include-utils \
  --extract-archives \
  --skip-initial-bootstrap \
  $SETUP_OPTS

# This uses gcc to compile tcc, so we must run the bootstrap script outside of
# the chroot since gcc isn't available in the chroot environment.
sh -c "cd $ROOTFS_DIR_GCC && sh jammed.sh && INSTALL_EXECS=0 BOOTSTRAP_SHELL=/bin/sh BOOTSTRAP_ARCH=$ARCH USE_GCC=1 sh bootstrap.sh"

checksum_result $ROOTFS_DIR_GCC > "$TEMP_DIR/checksums-gcc$ARCH_SUFFIX"

{ diff "$TEMP_DIR/checksums-pnut$ARCH_SUFFIX" "$TEMP_DIR/checksums-gcc$ARCH_SUFFIX" && echo "Checksums match, bootstrap is reproducible."; } \
  || { echo "Checksums do not match, bootstrap is not reproducible."; exit 1; }
