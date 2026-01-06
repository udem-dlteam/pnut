#! /bin/sh
# setup-rootfs.sh: Setup a minimal root filesystem for bootstrapping pnut
#
# Example usage:
#   ./kit/setup-rootfs.sh --dir island --jammed-path build/kit/jammed.sh
# Options:
#   --dir <path>: Path to create the root filesystem in (required)
#   --path-to-jammed <path>: Path to the jammed.sh script (required)
#   --bootstrap-shell <shell_name>: Name of the bootstrap shell to use (default: bash)
#   --include-utils: Include some scripts outside the jammed archive.
#   --skip-shell-bootstrap: Skip bootstrapping pnut-exe from the shell

set -e -u

error() {
  printf "Error: %s\n" "$1" >&2
  exit 1
}

readonly TEMP_DIR="build/kit"

CHROOT_DIR_NAME=""
BOOTSTRAP_SHELL="bash"
JAMMED_PATH=
INCLUDE_UTILS=0
SKIP_SHELL_BOOTSTRAP=0

while [ $# -gt 0 ]; do
  case $1 in
    --dir)
      if [ $# -lt 2 ]; then error "Missing argument for --dir option."; fi
      CHROOT_DIR_NAME="$2"
      shift 2
      ;;
    --bootstrap-shell)
      if [ $# -lt 2 ]; then error "Missing argument for --bootstrap-shell option."; fi
      BOOTSTRAP_SHELL="$2"
      shift 2
      ;;
    --path-to-jammed)
      if [ $# -lt 2 ]; then error "Missing argument for --path-to-jammed option."; fi
      JAMMED_PATH="$2"
      shift 2
      ;;
    --include-utils)
      INCLUDE_UTILS=1
      shift 1
      ;;
    --skip-shell-bootstrap)
      SKIP_SHELL_BOOTSTRAP=1
      shift 1
      ;;
    *) error "Unknown option: $1";;
  esac
done

if [ -z "$CHROOT_DIR_NAME" ]; then error "The --dir option is required."; fi
if [ -d "$CHROOT_DIR_NAME" ]; then error "Directory $CHROOT_DIR_NAME already exists."; fi
if [ -z "$JAMMED_PATH" ];     then error "The --path-to-jammed option is required."; fi
if [ ! -f "$JAMMED_PATH" ];   then error "Jammed script $JAMMED_PATH does not exist."; fi

mkdir -p "$TEMP_DIR"

# Create statically linked shell executables
# In the future, we'll add other shells as well
case $BOOTSTRAP_SHELL in
  "bash")
    BOOTSTRAP_SHELL_PATH=$(./kit/build-shells/bash.sh --print-path) ;;
  *)
    echo "Error: Unsupported bootstrap shell: $BOOTSTRAP_SHELL"
    exit 1
    ;;
esac

# The root filesystem has the following structure:
# - /bin directory with the bootstrap shell
# - /tmp directory for temporary files (created by bash sometimes)
# - jammed.sh script

mkdir -p "$CHROOT_DIR_NAME"
mkdir -p "$CHROOT_DIR_NAME/bin"
mkdir -p "$CHROOT_DIR_NAME/tmp"
chmod 1777 "$CHROOT_DIR_NAME/tmp" # Sticky bit for /tmp

cp "$BOOTSTRAP_SHELL_PATH" "$CHROOT_DIR_NAME/bin/$BOOTSTRAP_SHELL"
# Create symlink for sh for convenience
ln -s "/bin/$BOOTSTRAP_SHELL" "$CHROOT_DIR_NAME/bin/sh"

# Copy jammed.sh
cp "$JAMMED_PATH" "$CHROOT_DIR_NAME/jammed.sh"
chmod +x "$CHROOT_DIR_NAME/jammed.sh"

# Optionally include some utility scripts
if [ $INCLUDE_UTILS -eq 1 ]; then
  cp utils/cat.sh   "$CHROOT_DIR_NAME/cat.sh"
  cp utils/ls.sh    "$CHROOT_DIR_NAME/ls.sh"
  cp utils/touch.sh "$CHROOT_DIR_NAME/touch.sh"
  cp utils/wc.sh    "$CHROOT_DIR_NAME/wc.sh"
fi

if [ $SKIP_SHELL_BOOTSTRAP -eq 1 ]; then
  echo "Skipping shell bootstrap as requested."
  # Prebuild pnut-exe with gcc to skip slow shell bootstrap
  echo "Prebuilding pnut-exe with gcc..."
  PNUT_EXE_OPTIONS="-Dtarget_i386_linux -DONE_PASS_GENERATOR -DSAFE_MODE"
  gcc pnut.c \
    $PNUT_EXE_OPTIONS \
    -o $TEMP_DIR/pnut-exe-by-gcc

  ./$TEMP_DIR/pnut-exe pnut.c \
     -DBOOTSTRAP_TCC \
     $PNUT_EXE_OPTIONS \
     -o "$CHROOT_DIR_NAME/pnut-exe"
fi

echo "Bootstrap environment setup. You can now chroot into $CHROOT_DIR_NAME and run the jammed script:"
echo "  sudo chroot $CHROOT_DIR_NAME /bin/$BOOTSTRAP_SHELL"
echo "  $ . jammed.sh"
echo "  $ INSTALL_EXECS=1 BOOTSTRAP_SHELL=$BOOTSTRAP_SHELL . bootstrap.sh"
