__c2i_0=48
__c2i_1=49
__c2i_2=50
__c2i_3=51
__c2i_4=52
__c2i_5=53
__c2i_6=54
__c2i_7=55
__c2i_8=56
__c2i_9=57
__c2i_a=97
__c2i_b=98
__c2i_c=99
__c2i_d=100
__c2i_e=101
__c2i_f=102
__c2i_g=103
__c2i_h=104
__c2i_i=105
__c2i_j=106
__c2i_k=107
__c2i_l=108
__c2i_m=109
__c2i_n=110
__c2i_o=111
__c2i_p=112
__c2i_q=113
__c2i_r=114
__c2i_s=115
__c2i_t=116
__c2i_u=117
__c2i_v=118
__c2i_w=119
__c2i_x=120
__c2i_y=121
__c2i_z=122
__c2i_A=65
__c2i_B=66
__c2i_C=67
__c2i_D=68
__c2i_E=69
__c2i_F=70
__c2i_G=71
__c2i_H=72
__c2i_I=73
__c2i_J=74
__c2i_K=75
__c2i_L=76
__c2i_M=77
__c2i_N=78
__c2i_O=79
__c2i_P=80
__c2i_Q=81
__c2i_R=82
__c2i_S=83
__c2i_T=84
__c2i_U=85
__c2i_V=86
__c2i_W=87
__c2i_X=88
__c2i_Y=89
__c2i_Z=90

decode_chars() {
  while [ $# -gt 0 ]; do
    case $1 in
      [a-zA-Z0-9]) code=$((__c2i_$1)) ;;
      ' ') code=32 ;;
      '!') code=33 ;;
      '"') code=34 ;;
      '#') code=35 ;;
      '$') code=36 ;;
      '%') code=37 ;;
      '&') code=38 ;;
      "'") code=39 ;;
      '(') code=40 ;;
      ')') code=41 ;;
      '*') code=42 ;;
      '+') code=43 ;;
      ',') code=44 ;;
      '-') code=45 ;;
      '.') code=46 ;;
      '/') code=47 ;;
      ':') code=58 ;;
      ';') code=59 ;;
      '<') code=60 ;;
      '=') code=61 ;;
      '>') code=62 ;;
      '?') code=63 ;;
      '@') code=64 ;;
      '[') code=91 ;;
      '\') code=92 ;;
      ']') code=93 ;;
      '^') code=94 ;;
      '_') code=95 ;;
      '`') code=96 ;;
      '{') code=123 ;;
      '|') code=124 ;;
      '}') code=125 ;;
      '~') code=126 ;;
      *)
        code=$(LC_CTYPE=C printf "%d" "'$1")
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
