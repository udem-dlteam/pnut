ascii_table_0=48
ascii_table_1=49
ascii_table_2=50
ascii_table_3=51
ascii_table_4=52
ascii_table_5=53
ascii_table_6=54
ascii_table_7=55
ascii_table_8=56
ascii_table_9=57
ascii_table_a=97
ascii_table_b=98
ascii_table_c=99
ascii_table_d=100
ascii_table_e=101
ascii_table_f=102
ascii_table_g=103
ascii_table_h=104
ascii_table_i=105
ascii_table_j=106
ascii_table_k=107
ascii_table_l=108
ascii_table_m=109
ascii_table_n=110
ascii_table_o=111
ascii_table_p=112
ascii_table_q=113
ascii_table_r=114
ascii_table_s=115
ascii_table_t=116
ascii_table_u=117
ascii_table_v=118
ascii_table_w=119
ascii_table_x=120
ascii_table_y=121
ascii_table_z=122
ascii_table_A=65
ascii_table_B=66
ascii_table_C=67
ascii_table_D=68
ascii_table_E=69
ascii_table_F=70
ascii_table_G=71
ascii_table_H=72
ascii_table_I=73
ascii_table_J=74
ascii_table_K=75
ascii_table_L=76
ascii_table_M=77
ascii_table_N=78
ascii_table_O=79
ascii_table_P=80
ascii_table_Q=81
ascii_table_R=82
ascii_table_S=83
ascii_table_T=84
ascii_table_U=85
ascii_table_V=86
ascii_table_W=87
ascii_table_X=88
ascii_table_Y=89
ascii_table_Z=90

char_to_int_lookup() {
  val=$((ascii_table_$1))
  if [ -z "$val" ] ; then
    char_to_int_code=$(LC_CTYPE=C printf "%d" "'$1")
  else
    char_to_int_code=$val
  fi
}

decode_chars() {
  while [ $# -gt 0 ]; do
    case $1 in
      # Match on all alphanumerical chars
      [a-zA-Z0-9])
        char_to_int_lookup $1
        ;;
      # Match on all other chars
      *)
        char_to_int_code=$(LC_CTYPE=C printf "%d" "'$1")
        ;;
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
  decode_chars "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" " " "!" "#" "$" "%" "&" "(" ")" "*" "+" "," "-" "." "/" ":" ";" "<" "=" ">" "?" "@" "[" "]" "^" "_" "`" "{" "|" "}" "~"
  i=$((i+1))
done
