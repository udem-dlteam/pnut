#! /bin/sh
# make-jammed.sh: simple wrapper over list-bootstrap-files.sh and jam.sh to
#                 create a self-extracting archive of the bootstrap files.
# Example usage:
#   ./kit/make-jammed.sh <list-bootstrap-files.sh options>

set -eu

# Fail during extract if the extract bit is not set.
cat << 'EOF'
#! /bin/sh
#
# Check that the script is executable.
# This is so the bootstrap script can steal its execute bit, since it's
# impossible to bootstrap the execute bit from the shell.
if [ ! -x "$0" ] && [ "$1" != "--force-no-exec" ]; then
  printf "Error: $0 is not executable.\n"
  printf "This script is intended to be run in a specific environment where it can execute successfully.\n"
  printf "Please ensure you are in an empty directory and have the necessary permissions.\n"
  printf "Run \`chmod +x $0\` and try again.\n"
  exit 126
fi

EOF

PNUT_OPTIONS="${PNUT_OPTIONS:-}" kit/list-bootstrap-files.sh $@ | ./utils/jam.sh
