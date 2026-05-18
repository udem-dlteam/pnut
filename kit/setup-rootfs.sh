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

CHROOT_DIR=""
BOOTSTRAP_SHELL="bash-static"
INCLUDE_UTILS=0
SKIP_SHELL_BOOTSTRAP=0
EXECUTE_BOOTSTRAP=0

while [ $# -gt 0 ]; do
  case $1 in
    --dir)
      if [ $# -lt 2 ]; then error "Missing argument for --dir option."; fi
      CHROOT_DIR="$2"
      shift 2
      ;;
    --bootstrap-shell)
      if [ $# -lt 2 ]; then error "Missing argument for --bootstrap-shell option."; fi
      BOOTSTRAP_SHELL="$2"
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
    --execute-bootstrap)
      # Executes the bootstrap scripts inside of the chroot after setup.
      EXECUTE_BOOTSTRAP=1
      shift 1
      ;;
    *) error "Unknown option: $1";;
  esac
done

if [ -z "$CHROOT_DIR" ];    then error "The --dir option is required."; fi
if [ -d "$CHROOT_DIR" ];    then error "Directory $CHROOT_DIR already exists."; fi

mkdir -p "$TEMP_DIR"

# The root filesystem has the following structure:
# - /bin directory with the bootstrap shell
# - /tmp directory for temporary files (created by bash sometimes)
# - /lib directory for the libc scavenged from old Linux distributions ISOs
# - jammed.sh script

mkdir -p "$CHROOT_DIR"
mkdir -p "$CHROOT_DIR/bin"
mkdir -p "$CHROOT_DIR/tmp"
mkdir -p "$CHROOT_DIR/lib"
chmod 1777 "$CHROOT_DIR/tmp" # Sticky bit for /tmp

# Create/copy shell executables. Either scavenge them from old Linux distribution ISOs or build them from source.
case $BOOTSTRAP_SHELL in
  "bash-static")
    BOOTSTRAP_SHELL_PATH=$(./kit/build-shells/bash.sh --print-path)
    cp "$BOOTSTRAP_SHELL_PATH" "$CHROOT_DIR/bin/$BOOTSTRAP_SHELL"
    # Create symlink for sh for convenience
    ln -s "/bin/$BOOTSTRAP_SHELL" "$CHROOT_DIR/bin/sh"
    SHELL_EXE="bash"
    ;;
  "bash-i386-woody")
    if [ ! -f "kit/scavenge-shells/bash-i386-woody.tar" ]; then
      echo "Please run kit/scavenge-shells/bash-i386-woody.sh to prepare the scavenged bash-2.05a from Debian 3.0."
      exit 1
    fi
    tar -xf "kit/scavenge-shells/bash-i386-woody.tar" -C "$TEMP_DIR"
    bash "$TEMP_DIR/install.sh" "$TEMP_DIR" "$CHROOT_DIR"
    PNUT_OPTIONS="-DRT_FREE_UNSETS_VARS_NOT -DSH_PRINTF_PERCENT_B_COMPAT"
    SHELL_EXE="bash"
    ;;
  "zsh-i386-sarge")
    if [ ! -f "kit/scavenge-shells/zsh-i386-sarge.tar" ]; then
      echo "Please run kit/scavenge-shells/zsh-i386-sarge.sh to prepare the scavenged zsh-4.2.5 from Debian 3.1."
      exit 1
    fi
    tar -xf "kit/scavenge-shells/zsh-i386-sarge.tar" -C "$TEMP_DIR"
    bash "$TEMP_DIR/install.sh" "$TEMP_DIR" "$CHROOT_DIR"
    PNUT_OPTIONS="-DRT_FREE_UNSETS_VARS_NOT"
    SHELL_EXE="zsh"
    ;;
  "ksh-i386-sarge")
    if [ ! -f "kit/scavenge-shells/ksh-i386-sarge.tar" ]; then
      echo "Please run kit/scavenge-shells/ksh-i386-sarge.sh to prepare the scavenged ksh-88-r6 from Debian 3.1."
      exit 1
    fi
    tar -xf "kit/scavenge-shells/ksh-i386-sarge.tar" -C "$TEMP_DIR"
    bash "$TEMP_DIR/install.sh" "$TEMP_DIR" "$CHROOT_DIR"
    PNUT_OPTIONS="-DRT_FREE_UNSETS_VARS_NOT -DSH_PRINTF_PERCENT_B_COMPAT"
    SHELL_EXE="ksh"
    ;;
  "dash-i386-lenny")
    if [ ! -f "kit/scavenge-shells/dash-i386-lenny.tar" ]; then
      echo "Please run kit/scavenge-shells/dash-i386-lenny.sh to prepare the scavenged dash-0.5.4 from Debian 5.0."
      exit 1
    fi
    tar -xf "kit/scavenge-shells/dash-i386-lenny.tar" -C "$TEMP_DIR"
    bash "$TEMP_DIR/install.sh" "$TEMP_DIR" "$CHROOT_DIR"
    PNUT_OPTIONS="-DNO_TERNARY_SUPPORT -DSH_PRINTF_PERCENT_B_COMPAT -DSH_SHORT_PRINTF_LINES"
    SHELL_EXE="dash"
    ;;
  *)
    echo "Error: Unsupported bootstrap shell: $BOOTSTRAP_SHELL"
    exit 1
    ;;
esac

# Prepare jammed.sh
PNUT_OPTIONS="${PNUT_OPTIONS:-}" ./kit/make-jammed.sh --include-utils > "$TEMP_DIR/jammed.sh"

# Copy jammed.sh
cp "kit/jammed.sh" "$CHROOT_DIR/jammed.sh"
chmod +x "$CHROOT_DIR/jammed.sh"

# Optionally include some utility scripts
if [ $INCLUDE_UTILS -eq 1 ]; then
  cp utils/cat.sh   "$CHROOT_DIR/cat.sh"
  cp utils/ls.sh    "$CHROOT_DIR/ls.sh"
  cp utils/touch.sh "$CHROOT_DIR/touch.sh"
  cp utils/wc.sh    "$CHROOT_DIR/wc.sh"
  cp utils/sift.sh  "$CHROOT_DIR/sift.sh"
  cp utils/more.sh  "$CHROOT_DIR/more.sh"
fi

if [ $SKIP_SHELL_BOOTSTRAP -eq 1 ]; then
  echo "Skipping shell bootstrap as requested."
  # Prebuild pnut-exe with gcc to skip slow shell bootstrap
  echo "Prebuilding pnut-exe with gcc..."
  PNUT_EXE_OPTIONS="-Dtarget_i386_linux -DONE_PASS_GENERATOR -DSAFE_MODE"
  gcc pnut.c \
    $PNUT_EXE_OPTIONS \
    -o $TEMP_DIR/pnut-exe-by-gcc

  ./$TEMP_DIR/pnut-exe-by-gcc pnut.c \
     -DBOOTSTRAP_TCC \
     $PNUT_EXE_OPTIONS \
     -o "$CHROOT_DIR/pnut-exe"
fi

if [ $EXECUTE_BOOTSTRAP -eq 1 ]; then
  echo "Executing bootstrap script inside chroot..."
  sudo chroot "$CHROOT_DIR" /bin/sh -c "sh jammed.sh && INSTALL_EXECS=1 BOOTSTRAP_SHELL=/bin/$SHELL_EXE sh bootstrap.sh"
else
  echo "Bootstrap environment setup. You can now chroot into $CHROOT_DIR and run the jammed script:"
  echo "  sudo chroot $CHROOT_DIR /bin/sh"
  echo "  $ sh jammed.sh"
  echo "  $ INSTALL_EXECS=1 BOOTSTRAP_SHELL=sh sh bootstrap.sh"
fi
