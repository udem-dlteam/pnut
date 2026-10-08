#! /bin/sh
# setup-rootfs.sh: Setup a minimal root filesystem for bootstrapping pnut
#
# Example usage:
#   ./kit/setup-rootfs.sh --dir island --bootstrap-shell bash-static --execute-bootstrap
# Options:
#   --dir <path>: Path to create the root filesystem in (required)
#   --bootstrap-shell <shell_name>: Name of the bootstrap shell to use (default: bash)
#   --skip-initial-bootstrap: Skip bootstrapping pnut-exe, by compiling it with
#                             the host C compiler instead
#   --bootstrap-from-c4: Bootstrap pnut-exe from c4 instead of from pnut-sh.sh
#   --c4 <path>: Use this prebuilt c4 executable with --bootstrap-from-c4.
#                Must be statically linked
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

# Host C compiler, used to build c4 and the prebuilt pnut-exe. It is exported so
# that kit/make-jammed.sh and kit/list-bootstrap-files.sh use the same compiler.
: ${CC:=gcc}
export CC

CHROOT_DIR=""
BOOTSTRAP_SHELL="bash-static"
SKIP_INITIAL_BOOTSTRAP=0
EXECUTE_BOOTSTRAP=0
BOOTSTRAP_FROM="shell"
C4_BINARY=""
JAMMED_OPTS=""

# c4 source, from the kit/bootstrap-C4 submodule.
readonly C4_SRC="kit/bootstrap-C4/c4-pnut/c4.c"

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
    --skip-initial-bootstrap)
      SKIP_INITIAL_BOOTSTRAP=1
      shift 1
      ;;
    --bootstrap-from-c4)
      BOOTSTRAP_FROM="c4"
      shift 1
      ;;
    --c4)
      if [ $# -lt 2 ]; then error "Missing argument for --c4 option."; fi
      C4_BINARY="$2"
      shift 2
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

if [ -z "$CHROOT_DIR" ]; then
  error "The --dir option is required."
fi
if [ -d "$CHROOT_DIR" ]; then
  error "Directory $CHROOT_DIR already exists."
fi
if [ $SKIP_INITIAL_BOOTSTRAP -eq 1 ] && [ "$BOOTSTRAP_FROM" = c4 ]; then
  error "--skip-initial-bootstrap and --bootstrap-from-c4 are mutually exclusive: the former uses a precompiled pnut-exe, the latter compiles it with c4."
fi
if [ -n "$C4_BINARY" ] && [ "$BOOTSTRAP_FROM" != c4 ]; then
  error "--c4 requires --bootstrap-from-c4."
fi
if [ -n "$C4_BINARY" ] && [ ! -x "$C4_BINARY" ]; then
  error "c4 executable not found or not executable: $C4_BINARY"
fi

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
    PNUT_SH_COMPAT_OPTIONS="-DRT_FREE_UNSETS_VARS_NOT -DSH_PRINTF_PERCENT_B_COMPAT"
    SHELL_EXE="bash"
    ;;
  "zsh-i386-sarge")
    if [ ! -f "kit/scavenge-shells/zsh-i386-sarge.tar" ]; then
      error "Please run kit/scavenge-shells/zsh-i386-sarge.sh to prepare the scavenged zsh-4.2.5 from Debian 3.1."
    fi
    tar -xf "kit/scavenge-shells/zsh-i386-sarge.tar" -C "$TEMP_DIR"
    bash "$TEMP_DIR/install.sh" "$TEMP_DIR" "$CHROOT_DIR"
    PNUT_SH_COMPAT_OPTIONS="-DRT_FREE_UNSETS_VARS_NOT"
    SHELL_EXE="zsh"
    ;;
  "ksh-i386-sarge")
    if [ ! -f "kit/scavenge-shells/ksh-i386-sarge.tar" ]; then
      error "Please run kit/scavenge-shells/ksh-i386-sarge.sh to prepare the scavenged ksh-88-r6 from Debian 3.1."
    fi
    tar -xf "kit/scavenge-shells/ksh-i386-sarge.tar" -C "$TEMP_DIR"
    bash "$TEMP_DIR/install.sh" "$TEMP_DIR" "$CHROOT_DIR"
    PNUT_SH_COMPAT_OPTIONS="-DRT_FREE_UNSETS_VARS_NOT -DSH_PRINTF_PERCENT_B_COMPAT"
    SHELL_EXE="ksh"
    ;;
  "dash-i386-lenny")
    if [ ! -f "kit/scavenge-shells/dash-i386-lenny.tar" ]; then
      error "Please run kit/scavenge-shells/dash-i386-lenny.sh to prepare the scavenged dash-0.5.4 from Debian 5.0."
    fi
    tar -xf "kit/scavenge-shells/dash-i386-lenny.tar" -C "$TEMP_DIR"
    bash "$TEMP_DIR/install.sh" "$TEMP_DIR" "$CHROOT_DIR"
    PNUT_SH_COMPAT_OPTIONS="-DNO_TERNARY_SUPPORT -DSH_PRINTF_PERCENT_B_COMPAT -DSH_SHORT_PRINTF_LINES"
    SHELL_EXE="dash"
    ;;
  *)
    error "Error: Unsupported bootstrap shell: $BOOTSTRAP_SHELL"
    ;;
esac

# The bootstrap shell is only used to run the bootstrap scripts and to extract
# the jammed.sh archive. When bootstrapping from c4, it doesn't compile pnut;
# c4 does. See BOOTSTRAP_FROM in kit/bootstrap.sh.
if [ "$BOOTSTRAP_FROM" = c4 ]; then
  # The shell compatibility flags only apply to pnut-sh.sh, and aren't needed
  # when bootstrapping from c4.
  PNUT_SH_COMPAT_OPTIONS=""
  # Pass --bootstrap-from-c4 to make-jammed.sh to include cpp.c instead of
  # pnut-sh.sh, while keeping the options given on the command line.
  JAMMED_OPTS="$JAMMED_OPTS --bootstrap-from-c4"

  if [ -z "$C4_BINARY" ]; then
    if [ ! -f "$C4_SRC" ]; then
      error "Missing $C4_SRC. Fetch the c4 submodule with: git submodule update --init kit/bootstrap-C4"
    fi
    # c4 must run inside the chroot, which has no system C library, so it has to
    # be statically linked.
    echo "Building statically linked c4 from $C4_SRC with $CC..."
    if ! "$CC" -static -w -o "$TEMP_DIR/c4" "$C4_SRC"; then
      error "Failed to build a static c4. Build one that runs without a system C library and pass it with --c4 <path>."
    fi
    C4_BINARY="$TEMP_DIR/c4"
  else
    echo "Using prebuilt c4 executable: $C4_BINARY"
  fi
  cp "$C4_BINARY" "$CHROOT_DIR/c4"
  chmod 755 "$CHROOT_DIR/c4"
fi

PNUT_OPTIONS="${PNUT_SH_COMPAT_OPTIONS:-}" ./kit/make-jammed.sh $JAMMED_OPTS > "$TEMP_DIR/jammed.sh"
cp "$TEMP_DIR/jammed.sh" "$CHROOT_DIR/jammed.sh"
chmod +x "$CHROOT_DIR/jammed.sh"

if [ $SKIP_INITIAL_BOOTSTRAP -eq 1 ]; then
  # Prebuild pnut-exe with the host C compiler to skip the slow shell bootstrap
  echo "Skipping the initial bootstrap by precompiling pnut-exe"
  # MUST BE KEPT IN SYNC WITH kit/bootstrap.sh
  readonly PNUT_ARCH=i386_linux
  readonly PNUT_EXE_OPTIONS="-Dtarget_$PNUT_ARCH -DONE_PASS_GENERATOR"
  readonly PNUT_EXE_TCC_OPTIONS="$PNUT_EXE_OPTIONS -DSUPPORT_EMULATED_INT64 -DUNDEFINED_LABELS_ARE_RUNTIME_ERRORS -DENABLE_PNUT_INLINE_INTERRUPT -DNO_BUILTIN_LIBC"
  "$CC" -std=c99 pnut.c \
    $PNUT_EXE_OPTIONS \
    -o $TEMP_DIR/pnut-exe-by-cc

  ./$TEMP_DIR/pnut-exe-by-cc pnut.c \
     $PNUT_EXE_TCC_OPTIONS \
     -o "$CHROOT_DIR/pnut-exe"
fi

if [ $EXECUTE_BOOTSTRAP -eq 1 ]; then
  echo "Executing bootstrap script inside chroot..."
  sudo chroot "$CHROOT_DIR" /bin/sh -c "sh jammed.sh && INSTALL_EXECS=1 BOOTSTRAP_SHELL=/bin/$SHELL_EXE BOOTSTRAP_FROM=$BOOTSTRAP_FROM sh bootstrap.sh"
else
  echo "You can now chroot into the bootstrap environment at $CHROOT_DIR and run the jammed script:"
  echo "  sudo chroot $CHROOT_DIR /bin/sh"
  echo "  $ sh jammed.sh"
  echo "  $ INSTALL_EXECS=0 BOOTSTRAP_SHELL=/bin/$SHELL_EXE BOOTSTRAP_FROM=$BOOTSTRAP_FROM sh bootstrap.sh"
fi
