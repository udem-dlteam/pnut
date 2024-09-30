str="VERY LONG STRING TO REDUCE THE OVERHEAD OF EVERYTHING ELSE. DID YOU KNOW THAT THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG?\n"

test_with() {
  hyperfine "$1 shell-benchmarks/bench-puts.sh '$str' 1000 $2 > /dev/null"
}

test_with "bash" "slow"
test_with "bash" "fast"

test_with "dash" "slow"
test_with "dash" "fast"

test_with "ksh" "slow"
test_with "ksh" "fast"

test_with "zsh" "slow"
test_with "zsh" "fast"
