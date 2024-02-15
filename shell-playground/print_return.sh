# Test if empty arguments are counted in #$ when unquoted
a=123

do_something() {
  a=456
  echo "$a"
}

b=$(do_something)
echo "a: $a, b: $b"

# Subshell
(
  do_something
)
echo "a: $a, b: $b"

do_something
echo "a: $a, b: $b"
