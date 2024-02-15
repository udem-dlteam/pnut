# Can we call a function in a while condition? Yes!
counter=5

decrement_counter() {
  _0result=$(($1 - 1))
  return
}

while decrement_counter $counter; : $(( counter = _0result )); [ 0 -ne $counter ]; do
  echo "While: $counter"
done

echo "End"
