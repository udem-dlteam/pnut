#!/bin/sh
set -e -u -f
LC_ALL=C

_main() {
  printf "Hello, world\n" 
}

# Runtime library
__code=0; # Success exit code
_main __code; exit $__code
