#!/bin/sh
set -e -u -f
LC_ALL=C

_main() {
  :
}

# Runtime library
__code=0; # Exit code
_main __code
exit $__code
