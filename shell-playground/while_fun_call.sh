# Can we call a function in a while condition? Yes!
counter=5

decrement_counter() {
  : $(( counter -= 1 ))
}

while decrement_counter $counter; [ 0 -ne $counter ]; do
  echo "While: $counter"
done

echo "End"
