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
UTILS_TO_KEEP="bash dash ls ksh tail"

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

# Create symlink for sh for convenience
# sudo ln -s /usr/bin/bash "$CHROOT_DIR_NAME/usr/bin/sh"

# sudo chroot "$CHROOT_DIR_NAME" /usr/bin/bash -c "ls -l /usr/bin"

CLONE_PNUT=0
PNUT_BRANCH="laurent/small-fixes-for-TCC"
if [ $CLONE_PNUT ]; then
  # Put pnut's code
  cd $CHROOT_DIR_NAME
  sudo mkdir -p pnut
  sudo chown -R $(whoami) pnut
  git clone --depth 1 https://github.com/udem-dlteam/pnut.git --branch "$PNUT_BRANCH" pnut
  cd ..
fi

# Everything that's required will be in the trousse directory
sudo mkdir -p "$CHROOT_DIR_NAME/trousse"
sudo chown -R $(whoami) "$CHROOT_DIR_NAME/trousse"
cp pnut/kit/jammed.sh "$CHROOT_DIR_NAME/trousse/jammed.sh"
chmod +x "$CHROOT_DIR_NAME/trousse/jammed.sh"

# Cheating a bit to go faster
gcc -o $CHROOT_DIR_NAME/trousse/pnut-exe pnut/pnut.c -Dtarget_i386_linux -DBOOTSTRAP_TCC -DSAFE_MODE -DONE_PASS_GENERATOR
