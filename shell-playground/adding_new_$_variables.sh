# What happens when you add new $ variables in a function?
local_scope2() {
  a=$1
  b=$2
  c=$3
  echo "Local 2: $a $b $c"
  set $((a + 1)) $((b + 1)) $((c + 1))
  set 4 5 6 7 8 9
  echo "Local 2: $1 $2 $3 $4 $5 $6"
}

local_scope1() {
  a=$1
  b=$2
  c=$3
  local_scope2 1 2 3
  a=$1
  b=$2
  c=$3
  d=$4
  e=$5
  echo "Local 1: $a $b $c $d $e"
}

local_scope1 7 8 9
