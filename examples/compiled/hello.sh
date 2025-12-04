#!/bin/sh
set -e -u -f
LC_ALL=C

_main() {
  printf "Hello, world\n"
}

#_ Runtime library
__code=0; # Exit code
_main __code
exit $__code
