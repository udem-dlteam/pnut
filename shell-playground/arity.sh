# Test if empty arguments are counted in #$ when unquoted
abc=
def=123

arity_test() {
  echo "Arity: $#"
}

echo "Without quotes"
arity_test $abc $def
echo "With quotes"
arity_test "$abc" "$def"
