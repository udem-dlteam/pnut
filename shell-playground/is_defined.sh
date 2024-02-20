# Note: Note POSIX compliant and does not work in dash.
# This code is only used in _show_heap to not show undefined memory locations.

_1000=1
# _1001=1
show_heap_ix=1000

is_memory_addr_defined() {
  location="_$1"
  eval "if [[ -z \${$location+x} ]]; then undefined=1; else undefined=0; fi"
  echo "is undefined? $undefined"
}

is_memory_addr_defined $show_heap_ix # Show 1
is_memory_addr_defined $((show_heap_ix + 1)) # Show 0
