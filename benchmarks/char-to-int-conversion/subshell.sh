# Methods:
# 0: With printf in subshell
# 1: With char_to_int
# 2: With char_to_int_fast
# 3: With char_to_int_lookup
# if [ $# -eq 0 ]; then
#   echo "Usage: $0 <method>"
#   exit
# fi
# METHOD=$1

# decode_char() {
#   case $METHOD in
#     0) decode_char_res=$(LC_CTYPE=C printf "%d" "'$1") ;;
#     1) char_to_int "$1"; decode_char_res=$char_to_int_code ;;
#     2) char_to_int_fast "$1"; decode_char_res=$char_to_int_code ;;
#     3) char_to_int_lookup "$1"; decode_char_res=$char_to_int_code ;;
#   esac
# }

# char_to_int() {
#   case $1 in
#     "0") char_to_int_code=48 ;;
#     "1") char_to_int_code=49 ;;
#     "2") char_to_int_code=50 ;;
#     "3") char_to_int_code=51 ;;
#     "4") char_to_int_code=52 ;;
#     "5") char_to_int_code=53 ;;
#     "6") char_to_int_code=54 ;;
#     "7") char_to_int_code=55 ;;
#     "8") char_to_int_code=56 ;;
#     "9") char_to_int_code=57 ;;
#     "a") char_to_int_code=97 ;;
#     "b") char_to_int_code=98 ;;
#     "c") char_to_int_code=99 ;;
#     "d") char_to_int_code=100 ;;
#     "e") char_to_int_code=101 ;;
#     "f") char_to_int_code=102 ;;
#     "g") char_to_int_code=103 ;;
#     "h") char_to_int_code=104 ;;
#     "i") char_to_int_code=105 ;;
#     "j") char_to_int_code=106 ;;
#     "k") char_to_int_code=107 ;;
#     "l") char_to_int_code=108 ;;
#     "m") char_to_int_code=109 ;;
#     "n") char_to_int_code=110 ;;
#     "o") char_to_int_code=111 ;;
#     "p") char_to_int_code=112 ;;
#     "q") char_to_int_code=113 ;;
#     "r") char_to_int_code=114 ;;
#     "s") char_to_int_code=115 ;;
#     "t") char_to_int_code=116 ;;
#     "u") char_to_int_code=117 ;;
#     "v") char_to_int_code=118 ;;
#     "w") char_to_int_code=119 ;;
#     "x") char_to_int_code=120 ;;
#     "y") char_to_int_code=121 ;;
#     "z") char_to_int_code=122 ;;
#     "A") char_to_int_code=65 ;;
#     "B") char_to_int_code=66 ;;
#     "C") char_to_int_code=67 ;;
#     "D") char_to_int_code=68 ;;
#     "E") char_to_int_code=69 ;;
#     "F") char_to_int_code=70 ;;
#     "G") char_to_int_code=71 ;;
#     "H") char_to_int_code=72 ;;
#     "I") char_to_int_code=73 ;;
#     "J") char_to_int_code=74 ;;
#     "K") char_to_int_code=75 ;;
#     "L") char_to_int_code=76 ;;
#     "M") char_to_int_code=77 ;;
#     "N") char_to_int_code=78 ;;
#     "O") char_to_int_code=79 ;;
#     "P") char_to_int_code=80 ;;
#     "Q") char_to_int_code=81 ;;
#     "R") char_to_int_code=82 ;;
#     "S") char_to_int_code=83 ;;
#     "T") char_to_int_code=84 ;;
#     "U") char_to_int_code=85 ;;
#     "V") char_to_int_code=86 ;;
#     "W") char_to_int_code=87 ;;
#     "X") char_to_int_code=88 ;;
#     "Y") char_to_int_code=89 ;;
#     "Z") char_to_int_code=90 ;;
#     " ") char_to_int_code=32 ;;
#     "!") char_to_int_code=33 ;;
#     "\"") char_to_int_code=34 ;;
#     "#") char_to_int_code=35 ;;
#     "$") char_to_int_code=36 ;;
#     "%") char_to_int_code=37 ;;
#     "\&") char_to_int_code=38 ;;
#     "'") char_to_int_code=39 ;;
#     "(") char_to_int_code=40 ;;
#     ")") char_to_int_code=41 ;;
#     "\*") char_to_int_code=42 ;;
#     "+") char_to_int_code=43 ;;
#     ",") char_to_int_code=44 ;;
#     "-") char_to_int_code=45 ;;
#     ".") char_to_int_code=46 ;;
#     "/") char_to_int_code=47 ;;
#     ":") char_to_int_code=58 ;;
#     ";") char_to_int_code=59 ;;
#     "<") char_to_int_code=60 ;;
#     "=") char_to_int_code=61 ;;
#     ">") char_to_int_code=62 ;;
#     "?") char_to_int_code=63 ;;
#     "@") char_to_int_code=64 ;;
#     "[") char_to_int_code=91 ;;
#     "\\") char_to_int_code=92 ;;
#     "]") char_to_int_code=93 ;;
#     "^") char_to_int_code=94 ;;
#     "_") char_to_int_code=95 ;;
#     "\`") char_to_int_code=96 ;;
#     "{") char_to_int_code=123 ;;
#     "|") char_to_int_code=124 ;;
#     "}") char_to_int_code=125 ;;
#     "~") char_to_int_code=126 ;;
#     *)   char_to_int_code=$(LC_CTYPE=C printf "%d" "'$1") ;;
#   esac
# }

# char_to_int_fast() {
#   case $1 in
#     [0-9]) char_to_int_code=$((48 + $1)) ;;
#     [a-z])
#       case $1 in
#         [a-m])
#           case $1 in
#             [a-f])
#               case $1 in
#                 [a-c])
#                   case $1 in
#                     a) char_to_int_code=97 ;;
#                     b) char_to_int_code=98 ;;
#                     c) char_to_int_code=99 ;;
#                   esac ;;
#                 [d-f])
#                   case $1 in
#                     d) char_to_int_code=100 ;;
#                     e) char_to_int_code=101 ;;
#                     f) char_to_int_code=102 ;;
#                   esac ;;
#               esac ;;
#             [g-m])
#               case $1 in
#                 [g-i])
#                   case $1 in
#                     g) char_to_int_code=103 ;;
#                     h) char_to_int_code=104 ;;
#                     i) char_to_int_code=105 ;;
#                   esac ;;
#                 [j-m])
#                   case $1 in
#                     j) char_to_int_code=106 ;;
#                     k) char_to_int_code=107 ;;
#                     l) char_to_int_code=108 ;;
#                     m) char_to_int_code=109 ;;
#                   esac ;;
#               esac ;;
#           esac ;;
#         [n-z])
#           case $1 in
#             [n-s])
#               case $1 in
#                 [n-p])
#                   case $1 in
#                     n) char_to_int_code=110 ;;
#                     o) char_to_int_code=111 ;;
#                     p) char_to_int_code=112 ;;
#                   esac ;;
#                 [q-s])
#                   case $1 in
#                     q) char_to_int_code=113 ;;
#                     r) char_to_int_code=114 ;;
#                     s) char_to_int_code=115 ;;
#                   esac ;;
#               esac ;;
#             [t-z])
#               case $1 in
#                 [t-v])
#                   case $1 in
#                     t) char_to_int_code=116 ;;
#                     u) char_to_int_code=117 ;;
#                     v) char_to_int_code=118 ;;
#                   esac ;;
#                 [w-z])
#                   case $1 in
#                     w) char_to_int_code=119 ;;
#                     x) char_to_int_code=120 ;;
#                     y) char_to_int_code=121 ;;
#                     z) char_to_int_code=122 ;;
#                   esac ;;
#               esac ;;
#           esac ;;
#         esac ;;
#     [A-Z])
#       case $1 in
#         [A-M])
#           case $1 in
#             [A-F])
#               case $1 in
#                 [A-C])
#                   case $1 in
#                     A) char_to_int_code=65 ;;
#                     B) char_to_int_code=66 ;;
#                     C) char_to_int_code=67 ;;
#                   esac ;;
#                 [D-F])
#                   case $1 in
#                     D) char_to_int_code=68 ;;
#                     E) char_to_int_code=69 ;;
#                     F) char_to_int_code=70 ;;
#                   esac ;;
#               esac ;;
#             [G-M])
#               case $1 in
#                 [G-I])
#                   case $1 in
#                     G) char_to_int_code=71 ;;
#                     H) char_to_int_code=72 ;;
#                     I) char_to_int_code=73 ;;
#                   esac ;;
#                 [J-M])
#                   case $1 in
#                     J) char_to_int_code=74 ;;
#                     K) char_to_int_code=75 ;;
#                     L) char_to_int_code=76 ;;
#                     M) char_to_int_code=77 ;;
#                   esac ;;
#               esac ;;
#           esac ;;
#         [N-Z])
#           case $1 in
#             [N-S])
#               case $1 in
#                 [N-P])
#                   case $1 in
#                     N) char_to_int_code=78 ;;
#                     O) char_to_int_code=79 ;;
#                     P) char_to_int_code=80 ;;
#                   esac ;;
#                 [Q-S])
#                   case $1 in
#                     Q) char_to_int_code=81 ;;
#                     R) char_to_int_code=82 ;;
#                     S) char_to_int_code=83 ;;
#                   esac ;;
#               esac ;;
#             [T-Z])
#               case $1 in
#                 [T-V])
#                   case $1 in
#                     T) char_to_int_code=84 ;;
#                     U) char_to_int_code=85 ;;
#                     V) char_to_int_code=86 ;;
#                   esac ;;
#                 [W-Z])
#                   case $1 in
#                     W) char_to_int_code=87 ;;
#                     X) char_to_int_code=88 ;;
#                     Y) char_to_int_code=89 ;;
#                     Z) char_to_int_code=90 ;;
#                   esac ;;
#               esac ;;
#           esac ;;
#         esac ;;
#     " ") char_to_int_code=32 ;;
#     "!") char_to_int_code=33 ;;
#     "\"") char_to_int_code=34 ;;
#     "#") char_to_int_code=35 ;;
#     "$") char_to_int_code=36 ;;
#     "%") char_to_int_code=37 ;;
#     "\&") char_to_int_code=38 ;;
#     "'") char_to_int_code=39 ;;
#     "(") char_to_int_code=40 ;;
#     ")") char_to_int_code=41 ;;
#     "\*") char_to_int_code=42 ;;
#     "+") char_to_int_code=43 ;;
#     ",") char_to_int_code=44 ;;
#     "-") char_to_int_code=45 ;;
#     ".") char_to_int_code=46 ;;
#     "/") char_to_int_code=47 ;;
#     ":") char_to_int_code=58 ;;
#     ";") char_to_int_code=59 ;;
#     "<") char_to_int_code=60 ;;
#     "=") char_to_int_code=61 ;;
#     ">") char_to_int_code=62 ;;
#     "?") char_to_int_code=63 ;;
#     "@") char_to_int_code=64 ;;
#     "[") char_to_int_code=91 ;;
#     "\\") char_to_int_code=92 ;;
#     "]") char_to_int_code=93 ;;
#     "^") char_to_int_code=94 ;;
#     "_") char_to_int_code=95 ;;
#     "\`") char_to_int_code=96 ;;
#     "{") char_to_int_code=123 ;;
#     "|") char_to_int_code=124 ;;
#     "}") char_to_int_code=125 ;;
#     "~") char_to_int_code=126 ;;
#     *)   char_to_int_code=$(LC_CTYPE=C printf "%d" "'$1") ;;
#   esac
# }

# ascii_table_0=48
# ascii_table_1=49
# ascii_table_2=50
# ascii_table_3=51
# ascii_table_4=52
# ascii_table_5=53
# ascii_table_6=54
# ascii_table_7=55
# ascii_table_8=56
# ascii_table_9=57
# ascii_table_a=97
# ascii_table_b=98
# ascii_table_c=99
# ascii_table_d=100
# ascii_table_e=101
# ascii_table_f=102
# ascii_table_g=103
# ascii_table_h=104
# ascii_table_i=105
# ascii_table_j=106
# ascii_table_k=107
# ascii_table_l=108
# ascii_table_m=109
# ascii_table_n=110
# ascii_table_o=111
# ascii_table_p=112
# ascii_table_q=113
# ascii_table_r=114
# ascii_table_s=115
# ascii_table_t=116
# ascii_table_u=117
# ascii_table_v=118
# ascii_table_w=119
# ascii_table_x=120
# ascii_table_y=121
# ascii_table_z=122
# ascii_table_A=65
# ascii_table_B=66
# ascii_table_C=67
# ascii_table_D=68
# ascii_table_E=69
# ascii_table_F=70
# ascii_table_G=71
# ascii_table_H=72
# ascii_table_I=73
# ascii_table_J=74
# ascii_table_K=75
# ascii_table_L=76
# ascii_table_M=77
# ascii_table_N=78
# ascii_table_O=79
# ascii_table_P=80
# ascii_table_Q=81
# ascii_table_R=82
# ascii_table_S=83
# ascii_table_T=84
# ascii_table_U=85
# ascii_table_V=86
# ascii_table_W=87
# ascii_table_X=88
# ascii_table_Y=89
# ascii_table_Z=90

# char_to_int_lookup() {
#   val=$((ascii_table_$1))
#   if [ -z "$val" ] ; then
#     char_to_int_code=$(LC_CTYPE=C printf "%d" "'$1")
#   else
#     char_to_int_code=$val
#   fi
# }

decode_chars() {
  while [ $# -gt 0 ]; do
    : $(LC_CTYPE=C printf "%d" "'$1")
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
  decode_chars "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
  i=$((i+1))
done
