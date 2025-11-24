#! /bin/sh
# Create a statically linked bash executable

set -e -u

BUILD_DIR="build/bash-static"

# Create build directory
mkdir -p "$BUILD_DIR"

# Skip if already built
if [ ! -f "$BUILD_DIR/bash" ]; then
  # Redirect output to stderr to avoid mixing with the output of --print-path
  {
    git clone https://git.savannah.gnu.org/git/bash.git "$BUILD_DIR" --depth 1
    cd "$BUILD_DIR"
    git checkout a8a1c2fac029404d3f42cd39f5a20f24b6e4fe4b # Bash 5.3 (29/06/2025)
    ./configure --enable-static-link
    make
    echo "Statically linked bash created at $BUILD_DIR/bash"
    cd .. # Return to previous directory
  } >&2
fi

"$BUILD_DIR/bash" --version >&2

if [ "${1-}" = "--print-path" ]; then
  echo "$BUILD_DIR/bash"
fi
