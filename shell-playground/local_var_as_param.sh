# Are $1, $2, $3 local to the function?
fun_1() {
  echo "fun_1 before call: $1 $2 $3"
  fun_2 4 5 6
  echo "fun_1 after call: $1 $2 $3"
}

fun_2() {
  echo "fun_2: $1 $2 $3"
}

_identity() {
  : $(( _x = "$1" ))
  : $(( _0result = _x ))
}

fun_1 1 2 3
