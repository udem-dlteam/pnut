#!/bin/sh

# set -x # Prints all commands run
debug=1
set -e # Exit on first error
set -u # Exit on using unset variable

# Opcodes
LEA=0; IMM=1; REF=2; JMP=3; JSR=4; BZ=5; BNZ=6; ENT=7; ADJ=8; LEV=9; LI=10; LC=11; SI=12; SC=13; PSH=14; OR=15; XOR=16; AND=17; EQ=18; NE=19; LT=20; GT=21; LE=22; GE=23; SHL=24; SHR=25; ADD=26; SUB=27; MUL=28; DIV=29; MOD=30; OPEN=31; READ=32; CLOS=33; PRTF=34; MALC=35; FREE=36; MSET=37; MCMP=38; EXIT=39;

# if [ $# -eq 0 ]; then
#   echo "Usage: $0 <op-file>"
# fi

# TODO: Should the stack grow up or down? Up makes more sense since the negative
# sign breaks the variable interpolation, so Down caps the stack size at whatever
# sp is initialized to.
# It grows down in the original C code, so I'll keep it that way for now.
initial_stack_pos=1000000
sp=$initial_stack_pos
push_stack() {
  : $((sp--))
  : $((_data_$sp=$1))
}
pop_stack() {
  : $((res = _data_$sp))
  : $((sp++))
}

dat=0
push_data() {
  : $((_data_$dat=$1))
  : $((dat++))
}
pop_data() {
  : $((dat--))
  : $((res = _data_$dat))
}

push_instr() {
  push_data $1
  # echo "Pushing $1"
  # : $((_instr_$instr=$1))
  # : $((instr++))
}
pop_instr() {
  pop_data
  # : $((instr--))
  # : $((res = _instr_$instr))
}

# Read op codes

src_buf=
get_char()                           # get next char from source into $char
{
  if [ -z "$src_buf" ] ; then        # need to get next line when buffer empty
    IFS=                             # don't split input
    if read -r src_buf ; then        # read next line into $src_buf
      if [ -z "$src_buf" ] ; then    # an empty line implies a newline character
        char=NEWLINE                 # next get_char call will read next line
        return
      fi
    else
      char=EOF                       # EOF reached when read fails
      return
    fi
  else
    src_buf="${src_buf#?}"           # remove the current char from $src_buf
    if [ -z "$src_buf" ] ; then      # end of line if the buffer is now empty
      char=NEWLINE
      return
    fi
  fi

  # code=$(LC_CTYPE=C printf "%d" "'$char") # convert to integer code
  # echo "get_char returns $char: $char"

  # current character is at the head of $src_buf

  char="$src_buf"                    # remember current buffer
  rest="${src_buf#?}"                # remove the first char
  char="${char%"$rest"}"             # remove all but first char
}

parse_identifier()
{
  while : ; do
    case "$char" in
      [0-9a-zA-Z_])
        token="$token$char"
        get_char
        ;;
      *)
        break
        ;;
    esac
  done
}

get_token() {
  value=

  while : ; do
    token=$char
    get_char

    case "$token" in
      ' '|NEWLINE)                   # skip whitespace
          # echo "Skipping whitespace"
          while : ; do
            case "$char" in
              ' ') get_char ;;
              NEWLINE) token=$char ; get_char ;;
              *) break ;;
            esac
          done
          ;;

      [0-9-])                         # parse integer
          # echo "Parsing integer $token"
          value="$token"
          token=INTEGER
          while : ; do
            case "$char" in
              [0-9])
                value="$value$char"
                get_char
                ;;
              *)
                break
                ;;
            esac
          done
          break
          ;;

      [a-zA-Z_])                     # parse identifier or C keyword
          parse_identifier
          # echo "Parsed indentifier: $token"
          value="$token"
          token=IDENTIFIER
          break
          ;;

        *)                             # all else is treated as single char token
          break                        # (if there is no such token the parser
          ;;                           # will detect it and give an error)

    esac
  done
}

get_num() {
  get_token
  if [ "$token" != "INTEGER" ] ; then
    echo "Expected number, got $token: $value"; exit 1;
  fi
}

read_data() {
  get_char
  get_num
  count=$value

  echo "Reading $value bytes of data"

  while [ "$count" != "0" ] ; do
    get_char
    code=$(LC_CTYPE=C printf "%d" "'$char") # convert to integer code
    # echo "$char: $code"
    push_data $code
    : $((count--))
  done

  # Repeat data
  # while [ "$dat" != "0" ] ; do
  #   pop_data
  #   echo $res
  # done
}

# Encode instructions to internal representation.
# To inspect instructions, use decode_instructions and print_instructions.
encode_instruction() {
  case "$1" in
    LEA)  res=$LEA ;;
    IMM)  res=$IMM ;;
    REF)  res=$REF ;;
    JMP)  res=$JMP ;;
    JSR)  res=$JSR ;;
    BZ)   res=$BZ ;;
    BNZ)  res=$BNZ ;;
    ENT)  res=$ENT ;;
    ADJ)  res=$ADJ ;;
    LEV)  res=$LEV ;;
    LI)   res=$LI ;;
    LC)   res=$LC ;;
    SI)   res=$SI ;;
    SC)   res=$SC ;;
    PSH)  res=$PSH ;;
    OR)   res=$OR ;;
    XOR)  res=$XOR ;;
    AND)  res=$AND ;;
    EQ)   res=$EQ ;;
    NE)   res=$NE ;;
    LT)   res=$LT ;;
    GT)   res=$GT ;;
    LE)   res=$LE ;;
    GE)   res=$GE ;;
    SHL)  res=$SHL ;;
    SHR)  res=$SHR ;;
    ADD)  res=$ADD ;;
    SUB)  res=$SUB ;;
    MUL)  res=$MUL ;;
    DIV)  res=$DIV ;;
    MOD)  res=$MOD ;;
    OPEN) res=$OPEN ;;
    READ) res=$READ ;;
    CLOS) res=$CLOS ;;
    PRTF) res=$PRTF ;;
    MALC) res=$MALC ;;
    FREE) res=$FREE ;;
    MSET) res=$MSET ;;
    MCMP) res=$MCMP ;;
    EXIT) res=$EXIT ;;
    *) echo "Unknown instruction $1" ; exit 1 ;;
  esac
  # echo "$1: $res"
}

# It's kind of silly that we have to list everything, but without array I'm not sure how else to do it
decode_instruction() {
  case "$1" in
    $LEA)  res=LEA ;;
    $IMM)  res=IMM ;;
    $REF)  res=REF ;;
    $JMP)  res=JMP ;;
    $JSR)  res=JSR ;;
    $BZ)   res=BZ ;;
    $BNZ)  res=BNZ ;;
    $ENT)  res=ENT ;;
    $ADJ)  res=ADJ ;;
    $LEV)  res=LEV ;;
    $LI)   res=LI ;;
    $LC)   res=LC ;;
    $SI)   res=SI ;;
    $SC)   res=SC ;;
    $PSH)  res=PSH ;;
    $OR)   res=OR ;;
    $XOR)  res=XOR ;;
    $AND)  res=AND ;;
    $EQ)   res=EQ ;;
    $NE)   res=NE ;;
    $LT)   res=LT ;;
    $GT)   res=GT ;;
    $LE)   res=LE ;;
    $GE)   res=GE ;;
    $SHL)  res=SHL ;;
    $SHR)  res=SHR ;;
    $ADD)  res=ADD ;;
    $SUB)  res=SUB ;;
    $MUL)  res=MUL ;;
    $DIV)  res=DIV ;;
    $MOD)  res=MOD ;;
    $OPEN) res=OPEN ;;
    $READ) res=READ ;;
    $CLOS) res=CLOS ;;
    $PRTF) res=PRTF ;;
    $MALC) res=MALC ;;
    $FREE) res=FREE ;;
    $MSET) res=MSET ;;
    $MCMP) res=MCMP ;;
    $EXIT) res=EXIT ;;
    *) echo "Unknown instruction code $1" ; exit 1 ;;
  esac
}

read_instructions() {
  get_token
  count=0
  while : ; do
    # echo "$token: $value"
    case "$token" in
      EOF) break ;;
      INTEGER) push_instr $value ;;
      IDENTIFIER) encode_instruction $value ; push_instr $res ;;
      *) echo "Unknown instruction $value" ; exit 1 ;;
    esac
    : $((count++))
    get_token
  done
  echo "Finished reading $count instructions"
}

# Useful for debugging
print_instructions() {
  instr=$instr_start

  while [ $instr -lt $last_instr ]; do
    ix=$instr
    : $((i = _data_$instr))
    : $((instr++))

    if [ $i -le $ADJ ] ; then

      : $((imm = _data_$instr))
      : $((instr++))
      decode_instruction $i
      echo "$ix: $res  $imm"
    else
      decode_instruction $i
      echo "$ix: $res"
    fi
  done
}

run_instructions() {
  while : ; do
    # echo "cycle = $cycle:\n    pc = $pc, sp = $sp, bp = $bp, a = $a"

    : $((i = _data_$pc))
    : $((pc++))

    debug_str=""
    instr_str=""

    if [ $i -le $ADJ ] ; then
      : $((imm = _data_$pc))
      : $((pc++))
      decode_instruction $i
      instr_str=$debug_str$(echo "$res  $imm")
    else
      decode_instruction $i
      instr_str=$debug_str$(echo "$res")
    fi

    if [ $debug -eq 1 ] ; then
      # State
      debug_str="$cycle> \n    $instr_str\n    pc = $pc, sp = $sp, bp = $bp, a = $a"
      # echo $debug_str
      # Show stack
      # Because the stack may contain undefined values, this code is incompatible with the set -u option
      stack_ix=$initial_stack_pos
      debug_str="$debug_str\n    Stack:"
      while [ $stack_ix -gt $sp ]; do
        : $((stack_ix--))
        debug_str="$debug_str\n        _data_$stack_ix = $((_data_$stack_ix))"
      done
      echo $debug_str
    fi

    # Infinite loop breaker
    if [ $cycle -gt 250 ] ; then
      echo "Too many instructions"
      exit 1;
    fi

    #  if (i == LEA) a = (int)(bp + *pc++);                             // load local address
    if [ $i -eq $LEA ] ; then
      a=$((bp + imm))
    #  if (i == IMM) a = *pc++;                                         // load global address or immediate
    elif [ $i -eq $IMM ] ; then
      a=$imm
    #  if (i == REF) a = *pc++;                                         // load global address
    elif [ $i -eq $REF ] ; then
      a=$imm
    #  if (i == JMP) pc = (int *)*pc;                                   // jump
    elif [ $i -eq $JMP ] ; then
      pc=$imm
    #  if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    elif [ $i -eq $JSR ] ; then
      push_stack $pc # Not (pc + 1) because we already incremented pc when getting imm
      pc=$imm
    #  if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    elif [ $i -eq $BZ ] ; then
      if [ $a -eq 0 ] ; then
        pc=$imm
      fi
    #  if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    elif [ $i -eq $BNZ ] ; then
      if [ $a -ne 0 ] ; then
        pc=$imm
      fi
    #  if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    elif [ $i -eq $ENT ] ; then
      push_stack $bp
      bp=$sp
      sp=$((sp - imm))
    #  if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    elif [ $i -eq $ADJ ] ; then
      sp=$((sp + imm))
    #  if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    elif [ $i -eq $LEV ] ; then
      sp=$bp
      bp=$((_data_$sp))
      sp=$((sp + 1))
      pc=$((_data_$sp))
      sp=$((sp + 1))
    #  if (i == LI)  a = *(int *)a;                                     // load int
    elif [ $i -eq $LI ] ; then
      : $((a = _data_$a))
    #  if (i == LC)  a = *(char *)a;                                    // load char
    elif [ $i -eq $LC ] ; then
      a=$((_data_$a))
    #  if (i == SI)  *(int *)*sp++ = a;                                 // store int
    elif [ $i -eq $SI ] ; then
      # echo "_data_$(_data_$sp)"
      push_stack $a
    #  if (i == SC)  a = *(char *)*sp++ = a;                            // store char
    elif [ $i -eq $SC ] ; then
      push_stack $a
    #  if (i == PSH) *--sp = a;                                         // push
    elif [ $i -eq $PSH ] ; then
      push_stack $a
    elif [ $i -eq $OR ] ; then
      pop_stack
      a=$((res | a))
    elif [ $i -eq $XOR ] ; then
      pop_stack
      a=$((res ^ a))
    elif [ $i -eq $AND ] ; then
      pop_stack
      a=$((res & a))
    elif [ $i -eq $EQ ] ; then
      pop_stack
      a=$((res == a))
    elif [ $i -eq $NE ] ; then
      pop_stack
      a=$((res != a))
    elif [ $i -eq $LT ] ; then
      pop_stack
      a=$((res < a))
    elif [ $i -eq $GT ] ; then
      pop_stack
      a=$((res > a))
    elif [ $i -eq $LE ] ; then
      pop_stack
      a=$((res <= a))
    elif [ $i -eq $GE ] ; then
      pop_stack
      a=$((res >= a))
    elif [ $i -eq $SHL ] ; then
      pop_stack
      a=$((res << a))
    elif [ $i -eq $SHR ] ; then
      pop_stack
      a=$((res >> a))
    elif [ $i -eq $ADD ] ; then
      pop_stack
      a=$((res + a))
    elif [ $i -eq $SUB ] ; then
      pop_stack
      a=$((res - a))
    elif [ $i -eq $MUL ] ; then
      pop_stack
      a=$((res * a))
    elif [ $i -eq $DIV ] ; then
      pop_stack
      a=$((res / a))
    elif [ $i -eq $MOD ] ; then
      pop_stack
      a=$((res % a))
    elif [ $i -eq $OPEN ] ; then
      pop_stack
      a=$(open $res $a)
    elif [ $i -eq $READ ] ; then
      pop_stack
      a=$(read $res $a)
    elif [ $i -eq $CLOS ] ; then
      pop_stack
      a=$(close $a)
    elif [ $i -eq $PRTF ] ; then
      pop_stack
      a=$(printf $res $a)
    elif [ $i -eq $MALC ] ; then
      a=$(malloc $a)
    elif [ $i -eq $FREE ] ; then
      free $a
    elif [ $i -eq $MSET ] ; then
      pop_stack
      a=$(memset $res $a)
    elif [ $i -eq $MCMP ] ; then
      pop_stack
      a=$(memcmp $res $a)
    elif [ $i -eq $EXIT ] ; then
      echo "exit($a) cycle = $cycle"
      exit $a
    else
      echo "unknown instruction = $i! cycle = $cycle"
      exit 1
    fi

    : $((cycle++))
  done
}

read_data
instr_start=$dat
get_num
main_addr=$value
echo "Instructions start at $main_addr"
read_instructions
last_instr=$dat
print_instructions

# sp=0;
pc=$main_addr; bp=$sp; a=0; cycle=0; # vm registers
i=0; t=0 # temps

# setup first stack frame
push_stack $EXIT # call exit if main returns
push_stack $PSH
t=$sp
push_stack $(($# + 1)) # argc
push_stack 0 # argv # TODO: We need pack the arguments into the heap and pass a pointer here
push_stack $t

run_instructions
