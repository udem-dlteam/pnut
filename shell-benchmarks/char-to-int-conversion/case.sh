decode_chars() {
  while [ $# -gt 0 ]; do
    case $1 in
      "0") code=48 ;;
      "1") code=49 ;;
      "2") code=50 ;;
      "3") code=51 ;;
      "4") code=52 ;;
      "5") code=53 ;;
      "6") code=54 ;;
      "7") code=55 ;;
      "8") code=56 ;;
      "9") code=57 ;;
      "a") code=97 ;;
      "b") code=98 ;;
      "c") code=99 ;;
      "d") code=100 ;;
      "e") code=101 ;;
      "f") code=102 ;;
      "g") code=103 ;;
      "h") code=104 ;;
      "i") code=105 ;;
      "j") code=106 ;;
      "k") code=107 ;;
      "l") code=108 ;;
      "m") code=109 ;;
      "n") code=110 ;;
      "o") code=111 ;;
      "p") code=112 ;;
      "q") code=113 ;;
      "r") code=114 ;;
      "s") code=115 ;;
      "t") code=116 ;;
      "u") code=117 ;;
      "v") code=118 ;;
      "w") code=119 ;;
      "x") code=120 ;;
      "y") code=121 ;;
      "z") code=122 ;;
      "A") code=65 ;;
      "B") code=66 ;;
      "C") code=67 ;;
      "D") code=68 ;;
      "E") code=69 ;;
      "F") code=70 ;;
      "G") code=71 ;;
      "H") code=72 ;;
      "I") code=73 ;;
      "J") code=74 ;;
      "K") code=75 ;;
      "L") code=76 ;;
      "M") code=77 ;;
      "N") code=78 ;;
      "O") code=79 ;;
      "P") code=80 ;;
      "Q") code=81 ;;
      "R") code=82 ;;
      "S") code=83 ;;
      "T") code=84 ;;
      "U") code=85 ;;
      "V") code=86 ;;
      "W") code=87 ;;
      "X") code=88 ;;
      "Y") code=89 ;;
      "Z") code=90 ;;
      " ") code=32 ;;
      "!") code=33 ;;
      "\"") code=34 ;;
      "#") code=35 ;;
      "$") code=36 ;;
      "%") code=37 ;;
      "\&") code=38 ;;
      "'") code=39 ;;
      "(") code=40 ;;
      ")") code=41 ;;
      "\*") code=42 ;;
      "+") code=43 ;;
      ",") code=44 ;;
      "-") code=45 ;;
      ".") code=46 ;;
      "/") code=47 ;;
      ":") code=58 ;;
      ";") code=59 ;;
      "<") code=60 ;;
      "=") code=61 ;;
      ">") code=62 ;;
      "?") code=63 ;;
      "@") code=64 ;;
      "[") code=91 ;;
      "\\") code=92 ;;
      "]") code=93 ;;
      "^") code=94 ;;
      "_") code=95 ;;
      "\`") code=96 ;;
      "{") code=123 ;;
      "|") code=124 ;;
      "}") code=125 ;;
      "~") code=126 ;;
      *)   code=$(LC_CTYPE=C printf "%d" "'$1") ;;
    esac
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
decode_chars 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' 'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z' 'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O' 'P' 'Q' 'R' 'S' 'T' 'U' 'V' 'W' 'X' 'Y' 'Z' '0' '1' '2' '3' '4' '5' '6' '7' '8' '9' ' ' '!' '#' '$' '%' '&' '(' ')' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>' '?' '@' '[' ']' '^' '_' '{' '|' '}' '~'
  i=$((i+1))
done
