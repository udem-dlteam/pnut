# Can we put multiple expressions in a $(()) block? Yes!
if : $(( x = 5 )); [ 0 -eq $(( y = 0 )) ]; then
  echo "Should execute"
  echo "x: $x"
  echo "y: $y"
fi

if : $(( x = 5 )); [ 0 -eq $(( y = 1 )) ]; then
  echo "Should not execute"
  echo "x: $x"
  echo "y: $y"
fi

x=0
while : $(( x = x + 1 )); [ 3 -ne $(( x - 1 )) ]; do
  echo "It even supports post increment"
  echo "x: $x"
done
