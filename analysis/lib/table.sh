# table.sh — tiny helper for building aligned text tables in POSIX shell.
#
# Columns stay aligned no matter how wide each field is, because fields are
# joined with a real tab and the table is rendered through `column -t`.
#
# Usage:
#   SCRIPT_DIR=$(dirname "$0")
#   . "$SCRIPT_DIR/lib/table.sh"
#
#   table_init Variant C SH Ratio
#   table_row pnut-sh  9171 6133 0.668
#   table_row pnut-awk 7459 5338 0.715
#   table_print

_TABLE_TAB=$(printf '\t')
_TABLE_DATA=""

# Join all arguments into one tab-separated line (printed to stdout).
_table_join() {
  _tj_result=""
  for _tj_field in "$@"; do
    if [ -z "$_tj_result" ]; then
      _tj_result="$_tj_field"
    else
      _tj_result="$_tj_result$_TABLE_TAB$_tj_field"
    fi
  done
  printf '%s' "$_tj_result"
}

# Start a new (empty) table with a header row.
# Call once before adding rows.
table_init() {
  _TABLE_DATA=$(_table_join "$@")
}

# Append a data row.
table_row() {
  _TABLE_DATA="$_TABLE_DATA
$(_table_join "$@")"
}

# Render the accumulated table with aligned columns.
table_print() {
  printf '%s\n' "$_TABLE_DATA" | column -t -s "$_TABLE_TAB"
}
