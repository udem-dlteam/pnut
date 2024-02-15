# Can we put multiple expressions in a $(()) block? Yes!
if [ 0 -eq $(( x = 5, y = 0 )) ]; then
  echo "Should execute"
  echo "x: $x"
  echo "y: $y"
fi

if [ 0 -eq $(( x = 5, y = 1 )) ]; then
  echo "Should not execute"
  echo "x: $x"
  echo "y: $y"
fi

x=0
while [ 3 -ne $(( x = x + 1, x - 1 )) ]; do
  echo "It even supports post increment"
  echo "x: $x"
done
