#! /bin/sh
# setup-rootfs.sh: Setup a minimal root filesystem for bootstrapping pnut
#
# Example usage:
#   ./kit/setup-rootfs.sh --dir island --bootstrap-shell bash-static --execute-bootstrap
# Options:
#   --dir <path>: Path to create the root filesystem in (required)
#   --bootstrap-shell <shell_name>: Name of the bootstrap shell to use (default: bash)
#   --skip-shell-bootstrap: Skip bootstrapping pnut-exe from the shell
#   --execute-bootstrap: Execute the bootstrap scripts inside the chroot after setup
#
# The following options are passed directly to make-jammed.sh and control which
# files are included in the jammed.sh archive seed:
#   --include-utils: Seed the environment with debugging scripts.
#   --extract-archives: Extract .tar.gz files instead of passing them to jam.sh
#   --mes-libc: Use mes libc instead of pnut libc (default: pnut libc)
#   --tcc-version: Version of tcc to use (default: 0.9.27)

set -e -u

error() {
  printf "Error: %s\n" "$1" >&2
  exit 1
}

readonly TEMP_DIR="build/kit"

CHROOT_DIR=""
BOOTSTRAP_SHELL="bash-static"
SKIP_SHELL_BOOTSTRAP=0
EXECUTE_BOOTSTRAP=0
JAMMED_OPTS=""

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
    --skip-shell-bootstrap)
      SKIP_SHELL_BOOTSTRAP=1
      shift 1
      ;;
    --execute-bootstrap)
      # Executes the bootstrap scripts inside of the chroot after setup.
      EXECUTE_BOOTSTRAP=1
      shift 1
      ;;

    # The following options are passed to make-jammed.sh to control which files
    # are included in the jammed.sh archive.
    --include-utils|--extract-archives|--mes-libc)
      # These options are passed to list-bootstrap-files.sh to control which files are included in the jammed.sh archive.
      # They are not used directly by this script, but are forwarded to list-bootstrap-files.sh when creating the jammed.sh archive.
      JAMMED_OPTS="$JAMMED_OPTS $1"
      shift 1
      ;;
    --tcc-version)
      if [ $# -lt 2 ]; then error "Missing argument for --tcc-version option."; fi
      JAMMED_OPTS="$JAMMED_OPTS $1 $2"
      shift 2
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
      error "Please run kit/scavenge-shells/bash-i386-woody.sh to prepare the scavenged bash-2.05a from Debian 3.0."
    fi
    tar -xf "kit/scavenge-shells/bash-i386-woody.tar" -C "$TEMP_DIR"
    bash "$TEMP_DIR/install.sh" "$TEMP_DIR" "$CHROOT_DIR"
    EXTRA_PNUT_OPTIONS="-DRT_FREE_UNSETS_VARS_NOT -DSH_PRINTF_PERCENT_B_COMPAT"
    SHELL_EXE="bash"
    ;;
  "zsh-i386-sarge")
    if [ ! -f "kit/scavenge-shells/zsh-i386-sarge.tar" ]; then
      error "Please run kit/scavenge-shells/zsh-i386-sarge.sh to prepare the scavenged zsh-4.2.5 from Debian 3.1."
    fi
    tar -xf "kit/scavenge-shells/zsh-i386-sarge.tar" -C "$TEMP_DIR"
    bash "$TEMP_DIR/install.sh" "$TEMP_DIR" "$CHROOT_DIR"
    EXTRA_PNUT_OPTIONS="-DRT_FREE_UNSETS_VARS_NOT"
    SHELL_EXE="zsh"
    ;;
  "ksh-i386-sarge")
    if [ ! -f "kit/scavenge-shells/ksh-i386-sarge.tar" ]; then
      error "Please run kit/scavenge-shells/ksh-i386-sarge.sh to prepare the scavenged ksh-88-r6 from Debian 3.1."
    fi
    tar -xf "kit/scavenge-shells/ksh-i386-sarge.tar" -C "$TEMP_DIR"
    bash "$TEMP_DIR/install.sh" "$TEMP_DIR" "$CHROOT_DIR"
    EXTRA_PNUT_OPTIONS="-DRT_FREE_UNSETS_VARS_NOT -DSH_PRINTF_PERCENT_B_COMPAT"
    SHELL_EXE="ksh"
    ;;
  "dash-i386-lenny")
    if [ ! -f "kit/scavenge-shells/dash-i386-lenny.tar" ]; then
      error "Please run kit/scavenge-shells/dash-i386-lenny.sh to prepare the scavenged dash-0.5.4 from Debian 5.0."
    fi
    tar -xf "kit/scavenge-shells/dash-i386-lenny.tar" -C "$TEMP_DIR"
    bash "$TEMP_DIR/install.sh" "$TEMP_DIR" "$CHROOT_DIR"
    EXTRA_PNUT_OPTIONS="-DNO_TERNARY_SUPPORT -DSH_PRINTF_PERCENT_B_COMPAT -DSH_SHORT_PRINTF_LINES"
    SHELL_EXE="dash"
    ;;
  *)
    error "Error: Unsupported bootstrap shell: $BOOTSTRAP_SHELL"
    ;;
esac

PNUT_OPTIONS="${EXTRA_PNUT_OPTIONS:-}" ./kit/make-jammed.sh $JAMMED_OPTS > "$TEMP_DIR/jammed.sh"
cp "$TEMP_DIR/jammed.sh" "$CHROOT_DIR/jammed.sh"
chmod +x "$CHROOT_DIR/jammed.sh"

if [ $SKIP_SHELL_BOOTSTRAP -eq 1 ]; then
  # Prebuild pnut-exe with gcc to skip slow shell bootstrap
  echo "Skipping shell bootstrap by precompiling pnut-exe"
  # MUST BE KEPT IN SYNC WITH kit/bootstrap.sh
  readonly PNUT_ARCH=i386_linux
  readonly PNUT_EXE_OPTIONS="$EXTRA_PNUT_OPTIONS -Dtarget_$PNUT_ARCH -DONE_PASS_GENERATOR"
  readonly PNUT_EXE_TCC_OPTIONS="$PNUT_EXE_OPTIONS -DSUPPORT_EMULATED_INT64 -DUNDEFINED_LABELS_ARE_RUNTIME_ERRORS -DENABLE_PNUT_INLINE_INTERRUPT -DNO_BUILTIN_LIBC"
  cc -std=c99 pnut.c \
    $PNUT_EXE_OPTIONS \
    -o $TEMP_DIR/pnut-exe-by-cc

  ./$TEMP_DIR/pnut-exe-by-cc pnut.c \
     -DBOOTSTRAP_TCC \
     $PNUT_EXE_TCC_OPTIONS \
     -o "$CHROOT_DIR/pnut-exe"
fi

if [ $EXECUTE_BOOTSTRAP -eq 1 ]; then
  echo "Executing bootstrap script inside chroot..."
  sudo chroot "$CHROOT_DIR" /bin/sh -c "sh jammed.sh && INSTALL_EXECS=1 BOOTSTRAP_SHELL=/bin/$SHELL_EXE sh bootstrap.sh"
else
  echo "You can now chroot into the bootstrap environment at $CHROOT_DIR and run the jammed script:"
  echo "  sudo chroot $CHROOT_DIR /bin/sh"
  echo "  $ sh jammed.sh"
  echo "  $ INSTALL_EXECS=0 BOOTSTRAP_SHELL=/bin/$SHELL_EXE sh bootstrap.sh"
fi
