#! /bin/sh
#
# Prepare an almost empty environment to demonstrate how pnut can bootstrap a full environment.
# Included tools are:
# - bash: a POSIX shell to run commands (any POSIX-compatible shell would do)
# - chmod: to bootstrap the execute bit
# - ls: for demonstration purposes
#
# It also includes the pnut-sh.sh script, as well as sources for pnut, a few utilities and TCC.

set -e -u

CHROOT_DIR_NAME=island
UTILS_TO_KEEP="bash dash chmod ls ksh tail"
BASIC_UTILS="chmod cat cp mkdir sha256sum simple-patch ungz untar"
# BASIC_UTILS_FILES="chmod cat cp mkdir sha256sum simple-patch ungz untar"

if [ -d "$CHROOT_DIR_NAME" ]; then
  echo "Directory $CHROOT_DIR_NAME already exists. Please remove it before running this script."
  exit 1
fi

if [ ! -e "$CHROOT_DIR_NAME-fresh" ]; then
  echo "Directory $CHROOT_DIR_NAME-fresh does not exist. Creating a fresh debootstrap environment."
  sudo debootstrap bookworm "$CHROOT_DIR_NAME" || {
    echo "Failed to debootstrap the base system. Please check your network connection and try again."
    exit 1
  }

  # Pre-install a few shells
  sudo chroot "$CHROOT_DIR_NAME" /usr/bin/bash -c "apt-get update && apt-get install -y ksh dash"

  sudo cp -r "$CHROOT_DIR_NAME" "$CHROOT_DIR_NAME-fresh"
else
  echo "Using pre-downloaded debootstrap folder $CHROOT_DIR_NAME-fresh"
  sudo cp -r "$CHROOT_DIR_NAME-fresh" "$CHROOT_DIR_NAME"
fi

# Create a copy of the tools that are kept
for util in $UTILS_TO_KEEP; do
  sudo cp "$CHROOT_DIR_NAME/usr/bin/${util}" "$CHROOT_DIR_NAME/$util"
done

sudo chroot "$CHROOT_DIR_NAME" /usr/bin/bash -c "rm usr/bin/*"

# Put back the utilities we want to keep
for util in $UTILS_TO_KEEP; do
  sudo mv "$CHROOT_DIR_NAME/$util" "$CHROOT_DIR_NAME/usr/bin/${util}"
done

sudo chroot "$CHROOT_DIR_NAME" /usr/bin/bash -c "ls -l /usr/bin"

# Put pnut's code
cd $CHROOT_DIR_NAME

sudo mkdir -p pnut
sudo chown -R $(whoami) pnut

# Copy pnut and kits
git clone --depth 1 https://github.com/udem-dlteam/pnut.git --branch laurent/small-fixes-for-TCC pnut

# Everything that's required will be in the trousse directory
sudo mkdir -p trousse
sudo chown -R $(whoami) trousse

# Copy pnut-sh.sh script
pwd
ls -l
sudo cp pnut/kit/pnut-sh.sh trousse/pnut-sh.sh

copy_c_file_and_dependencies() {
  local file="$1"
  local dest_dir="$2"
  local comp_options="$3"

  # Create the destination directory if it doesn't exist
  mkdir -p "$dest_dir"

  # Get the dependencies of the C file
  local dependencies=$(gcc -MM "$file" $comp_options | tr ':' '\n' | tr '\\' ' ' | sed '1d')

  # Copy the C file and its dependencies to the destination directory
  for dep in $dependencies; do
    if [ -e "$dep" ]; then
      sudo cp "$dep" "$dest_dir/"
    else
      echo "Warning: $dep does not exist, skipping."
    fi
  done
}

# And copy the files for pnut-exe
copy_c_file_and_dependencies "pnut/pnut.c" "trousse/pnut_src" "-Dtarget_i386_linux -DSAFE_MODE -DBOOTSTRAP_TCC"

# Cheating a bit to go faster
# gcc -o trousse/pnut-exe-gcc trousse/pnut_src/pnut.c -Dtarget_i386_linux -DBOOTSTRAP_TCC -DSAFE_MODE -DONE_PASS_GENERATOR

# And the libc
sudo cp -r pnut/portable_libc trousse/

# With pnut-exe and the libc, we can compile some basic tools
mkdir -p trousse/utils
for util in $BASIC_UTILS; do
  copy_c_file_and_dependencies "pnut/kit/$util.c" "trousse/utils" ""
done

# Add a basic script named build-tools.sh to help compile the utils
cat > trousse/utils/build-tools.sh << EOF
#! /bin/sh
# This script compiles the tools needed to build the rest of the system.
# Usage: ./bootstrap-tools.sh {path_to_pnut_exe}

set -e -u

if [ \$# -ne 1 ]; then
  echo "Usage: \$0 {path_to_pnut_exe}"
  exit 1
fi

PNUT_EXE="\$1"
TOOLS_REPO="utils"

compile_with_pnut() {
  echo "Compiling \$1 with pnut-exe..."
  \$PNUT_EXE utils/\$1.c -I portable_libc/include/ portable_libc/libc.c -o utils/\$1
  if [ \$? -ne 0 ]; then
    printf "Failed to compile %s.c" "\$1"
    exit 1
  fi
}

# Note that these executables are placed in the kit directory because we don't
# have mkdir yet and so we cannot create the build directory.
for tool in $BASIC_UTILS; do
  compile_with_pnut "\$tool"
done

# Install tools in /usr/bin
for tool in $BASIC_UTILS; do
  echo "Installing utils/\$tool in /usr/bin/\$tool. Remember to then call chmod 755!"
  ./utils/cp utils/\$tool "/usr/bin/\$tool"
done
EOF

# And Mes and TCC's archive
mkdir -p trousse/files
# sudo cp pnut/kit/tcc-0.9.27.tar.gz trousse/tcc-0.9.27.tar.gz
sudo cp pnut/kit/tcc-0.9.26.tar.gz trousse/files/tcc-0.9.26.tar.gz
sudo cp pnut/kit/mes-0.27.tar.gz trousse/files/mes-0.27.tar.gz

sudo cp pnut/kit/mes-config.h trousse/files/mes-config.h

# Copy patches required for TCC
mkdir -p trousse/files/tcc-patches
sudo cp ../live-bootstrap/steps/tcc-0.9.26/simple-patches/* trousse/files/tcc-patches/

pwd
cp ../pnut/kit/bootstrap-tcc.sh trousse/bootstrap-tcc.sh
