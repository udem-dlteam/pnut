char_to_int() {
  case $1 in
    "0") char_to_int_code=48 ;;
    "1") char_to_int_code=49 ;;
    "2") char_to_int_code=50 ;;
    "3") char_to_int_code=51 ;;
    "4") char_to_int_code=52 ;;
    "5") char_to_int_code=53 ;;
    "6") char_to_int_code=54 ;;
    "7") char_to_int_code=55 ;;
    "8") char_to_int_code=56 ;;
    "9") char_to_int_code=57 ;;
    "a") char_to_int_code=97 ;;
    "b") char_to_int_code=98 ;;
    "c") char_to_int_code=99 ;;
    "d") char_to_int_code=100 ;;
    "e") char_to_int_code=101 ;;
    "f") char_to_int_code=102 ;;
    "g") char_to_int_code=103 ;;
    "h") char_to_int_code=104 ;;
    "i") char_to_int_code=105 ;;
    "j") char_to_int_code=106 ;;
    "k") char_to_int_code=107 ;;
    "l") char_to_int_code=108 ;;
    "m") char_to_int_code=109 ;;
    "n") char_to_int_code=110 ;;
    "o") char_to_int_code=111 ;;
    "p") char_to_int_code=112 ;;
    "q") char_to_int_code=113 ;;
    "r") char_to_int_code=114 ;;
    "s") char_to_int_code=115 ;;
    "t") char_to_int_code=116 ;;
    "u") char_to_int_code=117 ;;
    "v") char_to_int_code=118 ;;
    "w") char_to_int_code=119 ;;
    "x") char_to_int_code=120 ;;
    "y") char_to_int_code=121 ;;
    "z") char_to_int_code=122 ;;
    "A") char_to_int_code=65 ;;
    "B") char_to_int_code=66 ;;
    "C") char_to_int_code=67 ;;
    "D") char_to_int_code=68 ;;
    "E") char_to_int_code=69 ;;
    "F") char_to_int_code=70 ;;
    "G") char_to_int_code=71 ;;
    "H") char_to_int_code=72 ;;
    "I") char_to_int_code=73 ;;
    "J") char_to_int_code=74 ;;
    "K") char_to_int_code=75 ;;
    "L") char_to_int_code=76 ;;
    "M") char_to_int_code=77 ;;
    "N") char_to_int_code=78 ;;
    "O") char_to_int_code=79 ;;
    "P") char_to_int_code=80 ;;
    "Q") char_to_int_code=81 ;;
    "R") char_to_int_code=82 ;;
    "S") char_to_int_code=83 ;;
    "T") char_to_int_code=84 ;;
    "U") char_to_int_code=85 ;;
    "V") char_to_int_code=86 ;;
    "W") char_to_int_code=87 ;;
    "X") char_to_int_code=88 ;;
    "Y") char_to_int_code=89 ;;
    "Z") char_to_int_code=90 ;;
    " ") char_to_int_code=32 ;;
    "!") char_to_int_code=33 ;;
    "\"") char_to_int_code=34 ;;
    "#") char_to_int_code=35 ;;
    "$") char_to_int_code=36 ;;
    "%") char_to_int_code=37 ;;
    "\&") char_to_int_code=38 ;;
    "'") char_to_int_code=39 ;;
    "(") char_to_int_code=40 ;;
    ")") char_to_int_code=41 ;;
    "\*") char_to_int_code=42 ;;
    "+") char_to_int_code=43 ;;
    ",") char_to_int_code=44 ;;
    "-") char_to_int_code=45 ;;
    ".") char_to_int_code=46 ;;
    "/") char_to_int_code=47 ;;
    ":") char_to_int_code=58 ;;
    ";") char_to_int_code=59 ;;
    "<") char_to_int_code=60 ;;
    "=") char_to_int_code=61 ;;
    ">") char_to_int_code=62 ;;
    "?") char_to_int_code=63 ;;
    "@") char_to_int_code=64 ;;
    "[") char_to_int_code=91 ;;
    "\\") char_to_int_code=92 ;;
    "]") char_to_int_code=93 ;;
    "^") char_to_int_code=94 ;;
    "_") char_to_int_code=95 ;;
    "\`") char_to_int_code=96 ;;
    "{") char_to_int_code=123 ;;
    "|") char_to_int_code=124 ;;
    "}") char_to_int_code=125 ;;
    "~") char_to_int_code=126 ;;
    *)   char_to_int_code=$(LC_CTYPE=C printf "%d" "'$1") ;;
  esac
}

decode_chars() {
  while [ $# -gt 0 ]; do
    char_to_int $1
    shift
  done
}

if [ $# -ne 2 ]; then
  echo "Usage: $0 <env_size> <number of iterations>"
  exit 1
fi

env_size=$1
iterations=$2

# Add some variables to the environment so subshells take longer to start
i=0
while [ $i -lt $env_size ]; do
  i=$((i+1))
  : $((acc_$i = 1))
done

i=0
while [ $i -lt $iterations ]; do
  decode_chars "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" " " "!" "#" "$" "%" "&" "(" ")" "*" "+" "," "-" "." "/" ":" ";" "<" "=" ">" "?" "@" "[" "]" "^" "_" "`" "{" "|" "}" "~"
  i=$((i+1))
done
