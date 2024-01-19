#!/bin/sh

# set -x # Prints all commands run
set -e # Exit on first error

debug=0       # Print debug information, mostly during initialization
trace=0       # Trace the execution of the VM
trace_stack=0 # Trace the stack when tracing instructions
trace_heap=1  # Trace the heap when tracing instructions
strict_mode=1 # Ensures that all variables are initialized before use
if [ $trace_stack -eq 0 ] && [ $strict_mode -eq 1 ] ; then
  set -u # Exit on using unset variable
fi
fast_buffer_len=50 # Length of the fast buffer used by get_char

# Infinite loop breaker.
# On a M1 CPU, 35518 cycles take 18 seconds to run, so 100000 is a minute of execution.
MAX_CYCLE=10000000

# Opcodes
LEA=0; IMM=1; REF=2; JMP=3; JSR=4; BZ=5; BNZ=6; ENT=7; ADJ=8; LEV=9; LI=10; LC=11; SI=12; SC=13; PSH=14; OR=15; XOR=16; AND=17; EQ=18; NE=19; LT=20; GT=21; LE=22; GE=23; SHL=24; SHR=25; ADD=26; SUB=27; MUL=28; DIV=29; MOD=30; OPEN=31; READ=32; CLOS=33; PRTF=34; MALC=35; FREE=36; MSET=37; MCMP=38; EXIT=39;

INITIAL_STACK_POS=10000000
INITIAL_HEAP_POS=0
sp=$INITIAL_STACK_POS
push_stack() {
  : $((sp -= 1))
  : $((_data_$sp=$1))
}
pop_stack() {
  : $((res = _data_$sp))
  : $((sp += 1))
}
at_stack() {
  : $((res = _data_$(($1 + $sp))))
}

alloc_memory() {
  res=$dat
  : $((dat += $1))
  # Need to initialize the memory to 0 or else `set -u` will complain
  if [ $strict_mode -eq 1 ] ; then
    ix=$res
    while [ $ix -lt $dat ]; do
      : $((_data_$ix=0))
      : $((ix += 1))
    done
  fi
}

dat=$INITIAL_HEAP_POS
push_data() {
  : $((_data_$dat=$1))
  : $((dat += 1))
}
pop_data() {
  : $((dat -= 1))
  : $((res = _data_$dat))
}

# Push a Shell string to the VM heap. Returns a reference to the string in $addr.
unpack_string() {
  addr=$dat
  src_buf="$1"
  while [ -n "$src_buf" ] ; do
    char="$src_buf"                    # remember current buffer
    rest="${src_buf#?}"                # remove the first char
    char="${char%"$rest"}"             # remove all but first char
    src_buf="${src_buf#?}"             # remove the current char from $src_buf
    code=$(LC_CTYPE=C printf "%d" "'$char'")
    push_data "$code"
  done
  push_data 0
}

# Convert a VM string reference to a Shell string.
# $res is set to the result, and $len is set to the length of the string.
pack_string() {
  addr="$1";  shift
  max_len=100000000
  delim=0
  if [ $# -ge 1 ] ; then delim="$1" ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then max_len="$1" ; shift ; fi # Optional max length
  len=0; res=""
  while [ "$((_data_$addr))" -ne $delim ] && [ $max_len -gt $len ] ; do
    char="$((_data_$addr))"
    addr=$((addr + 1))
    len=$((len + 1))
    case $char in
      10) res="$res\n" ;; # 10 == '\n'
      *) res=$res$(printf "\\$(printf "%o" "$char")") # Decode
    esac
  done
}

# C like printf function.
# Arg 1 is the number of arguments to printf.
# Arg 2 is the format string.
# Arg 3 to n are the arguments to printf.
# Supports the following format specifiers:
#   %d - decimal integer
#   %s - string
#   %c - character
#   %x - hexadecimal integer
#   %.*s - string with length
#   %n.ms - string with length (n) and padding (m)
# Shell's printf function cannot be used because it does not support the %.*s
# format specifier and the strings passed to it need to be unpacked.
c_printf() {
  count="$1"; shift
  fmt_ptr=$1; shift
  arg_offset=2 # First argument is at stack position ($count - 2)
  str=""
  mod=0
  while [ "$((_data_$fmt_ptr))" -ne 0 ] ; do
    head="$((_data_$fmt_ptr))"
    head_char=$(printf "\\$(printf "%o" "$head")") # Decode
    fmt_ptr=$((fmt_ptr + 1))
    if [ $mod -eq 1 ] ; then
      case $head_char in
        'd') # 100 = 'd' Decimal integer
          at_stack $((count - arg_offset)); : $((arg_offset = arg_offset + 1)); imm=$res
          str="$str$imm"
          ;;
        'c') # 99 = 'c' Character
          at_stack $((count - arg_offset)); : $((arg_offset = arg_offset + 1)); char=$res
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          str="$str$(printf "\\$(printf "%o" "$char")")"
          ;;
        'x') # 120 = 'x' Hexadecimal integer
          at_stack $((count - arg_offset)); : $((arg_offset = arg_offset + 1)); imm=$res
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          str="$str$(printf "%x" "$imm")"
          ;;
        's') # 115 = 's' String
          at_stack $((count - arg_offset)); : $((arg_offset = arg_offset + 1)); str_ref=$res
          pack_string "$str_ref" # result in $res
          str="$str$res"
          ;;
        '.') # String with length
          pack_string $fmt_ptr 0 2 # Read next 2 characters
          fmt_ptr=$((fmt_ptr + 2))
          if [ "$res" = "*s" ]; then
            at_stack $((count - arg_offset)); : $((arg_offset = arg_offset + 1)); len=$res
            at_stack $((count - arg_offset)); : $((arg_offset = arg_offset + 1)); str_ref=$res
            pack_string $str_ref 0 $len # result in $res
            str="$str$res"
          else
            echo "Unknown format specifier: %.$res" ; exit 1
          fi
          ;;
        [0-9])                         # parse integer
          # Get max length (with padding)
          pack_string $fmt_ptr 46 # Read until '.' or end of string
          fmt_ptr=$((fmt_ptr + len + 1))
          min_len=$head_char$res # Don't forget the first digit we've already read

          # Get string length
          pack_string $fmt_ptr 115 # Read until 's' or end of string
          fmt_ptr=$((fmt_ptr + len))
          str_len=$res

          head="$((_data_$fmt_ptr))"
          head_char=$(printf "\\$(printf "%o" "$head")") # Decode
          fmt_ptr=$((fmt_ptr + 1))
          if [ "$head_char" = 's' ]; then
            at_stack $((count - arg_offset)); : $((arg_offset = arg_offset + 1)); str_ref=$res
            pack_string $str_ref 0 $str_len # result in $res
              : $((padding_len = $min_len - $len))
            while [ $padding_len -gt 0 ]; do # Pad string so it has at least $min_len characters
              res=" $res"
                : $((padding_len -= 1))
              done
            str="$str$res"
          else
            echo "Unknown format specifier: '%$min_len.$str_len$head_char'" ; exit 1;
          fi
          ;;
        *)
          echo "Unknown format specifier %$head_char"; exit 1
          str="$str$head_char" ;;
      esac
      mod=0
    else
      case $head in
        10) str="$str\n" ;; # 10 == '\n'
        37) mod=1 ;; # 37 == '%'
        *) str="$str$head_char" ;; # Decode
      esac
    fi
  done
  printf "%s" "$str"
}

# Read a character from stdin.
# Uses a buffer for the line, and a smaller fixed-sized buffer to speed up
# reading of long lines. The fixed-sized buffer is refilled from the line buffer
# when it is empty.
# This optimization is necessary because the string operations of all shell
# scale linearly with the size of the string, meaning that removing the head
# char from a string of length N takes O(N) time. Since get_char is called for
# every character in the line, reading a line of length N takes O(N^2) time.
# This optimization doesn't change the complexity but reduces the constant
# factor.
src_buf=
src_buf_fast=
get_char()                           # get next char from source into $char
{
  if [ -z "$src_buf_fast" ] && [ -z "$src_buf" ] ; then
    IFS=                             # don't split input
    if read -r src_buf ; then        # read next line into $src_buf
      if [ -z "$src_buf" ] ; then    # an empty line implies a newline character
        char=NEWLINE                 # next get_char call will read next line
        return
      fi
      src_buf_fast=$(printf "%.${fast_buffer_len}s" "$src_buf") # Copy first 100 chars to fast buffer
      src_buf="${src_buf#"$src_buf_fast"}"       # remove first 100 chars from $src_buf
    else
      char=EOF                       # EOF reached when read fails
      return
    fi
  elif [ -z "$src_buf_fast" ] || [ -z "${src_buf_fast#?}" ]; then      # need to refill fast buffer
    src_buf_fast=$(printf "%.${fast_buffer_len}s" "$src_buf") # Copy first 100 chars to fast buffer
    src_buf="${src_buf#"$src_buf_fast"}"       # remove first 100 chars from $src_buf
    if [ -z "$src_buf_fast" ] ; then           # end of line if the buffer is now empty
      char=NEWLINE
      return
    fi
    char=$src_buf_fast                    # remember current buffer
    rest=${src_buf_fast#?}                # remove the first char
    char=${char%"$rest"}                  # remove all but first char
  else # src_buf_fast is not empty and doesn't contain only a newline
    src_buf_fast="${src_buf_fast#?}"      # remove the current char from $src_buf
  fi

  # current character is at the head of $src_buf_fast
  char=$src_buf_fast                    # remember current buffer
  rest=${src_buf_fast#?}                # remove the first char
  char=${char%"$rest"}                  # remove all but first char
}

# Used to implement the read instruction.
# Does not work with NUL characters.
read_n_char() {
  count=$1
  buf_ptr=$2
  while [ "$count" != "0" ] ; do
    get_char
    case "$char" in
      EOF) break ;;
      NEWLINE) code=10 ;; # 10 == '\n'
      *) code=$(LC_CTYPE=C printf "%d" "'$char") # convert to integer code ;;
    esac

    : $((_data_$buf_ptr=$code))
    : $((count -= 1))
    : $((buf_ptr += 1))
  done

  : $((_data_$buf_ptr=0))
}

# Same as get_char, but \ is recognized as the start of an escape sequence.
# If an escape sequence is found, $char is set its ascii code and $escaped is set to true.
# If the next character is not \, the function behaves just like get_char.
get_char_encoded() {
  get_char
  escaped="false"
  # If $char is \, then the next 2 characters are the hex code of the character
  if [ '\' = "$char" ] ; then
    escaped="true"
    get_char
    x1=$char
    get_char
    x2=$char
    : $((char=0x$x1$x2))
    return
  fi
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
          while : ; do
            case "$char" in
              ' ') get_char ;;
              NEWLINE) token=$char ; get_char ;;
              *) break ;;
            esac
          done
          ;;

      [0-9-])                         # parse integer
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

  if [ $debug -eq 1 ] ; then
    echo "Reading $value bytes of data"
  fi

  while [ "$count" != "0" ] ; do
    get_char_encoded
    if [ $escaped = "true" ] ; then
      code=$char
    else
    code=$(LC_CTYPE=C printf "%d" "'$char") # convert to integer code
    fi
    push_data $code
    : $((count -= 1))
  done

  # Read final newline
  get_char
}

# Encode instructions to internal representation.
# To inspect instructions, use decode_instructions and print_instructions.
encode_instruction() {
  # This big case statement could be replaced with res=$(( $(($1)) )) but it
  # wouldn't handle the case where $1 is an invalid instruction, which is useful
  # for debugging.
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
}

# Without arrays it's hard to write this function in a way that isn't verbose.
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

# Read instructions and encode them until EOF.
read_instructions() {
  get_token
  count=0
  while : ; do
    case "$token" in
      EOF) break ;;
      INTEGER)
        if [ $patch_next_imm = "true" ] ; then
          value=$(($value + $patch))
        fi
        push_data $value ;;
      IDENTIFIER) encode_instruction $value ; push_data $res ;;
      *) echo "Unknown instruction $value" ; exit 1 ;;
    esac
    # Because instructions with relative addresses are relative to 0, we need to
    # patch them to be relative to the start of the instructions.
    case "$value" in
      "REF") patch_next_imm="true" ; patch=$1 ;;
      "JMP") patch_next_imm="true" ; patch=$2 ;;
      "JSR") patch_next_imm="true" ; patch=$2 ;;
      "BZ")  patch_next_imm="true" ; patch=$2 ;;
      "BNZ") patch_next_imm="true" ; patch=$2 ;;
      *)     patch_next_imm="false"; ;;
    esac
    : $((count += 1))
    get_token
  done
  if [ $debug -eq 1 ] ; then
    echo "Finished reading $count instructions"
  fi
}

# Useful for debugging
print_instructions() {
  echo "Main starts at position $main_addr"
  instr=$instr_start

  while [ $instr -lt $last_instr ]; do
    ix=$instr
    : $((i = _data_$instr))
    : $((instr += 1))

    if [ $i -le $ADJ ] ; then
      : $((imm = _data_$instr))
      : $((instr += 1))
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
    : $((i = _data_$pc))
    : $((pc += 1))
    : $((cycle += 1))

    if [ $i -le $ADJ ] ; then
      : $((imm = _data_$pc))
      : $((pc += 1))
    fi

    if [ $trace -eq 1 ] ; then
      debug_str=""
      instr_str=""
      # Current instruction
      decode_instruction $i
      if [ $i -le $ADJ ] ; then
        instr_str="$res  $imm"
      else
        instr_str="$res"
      fi

      # VM registers
      echo "$cycle>\n    $instr_str"
      show_vm_state
      if [ $trace_stack -eq 1 ] ; then show_stack ; fi
      if [ $trace_heap -eq 1 ] ; then show_heap ; fi
    fi

    # Infinite loop breaker
    if [ $cycle -gt $MAX_CYCLE ] ; then
      echo "Too many instructions, aborting execution."
      exit 1;
    fi

    case "$i" in
      "$LEA") a=$((bp + imm)) ;;                # a = (int)(bp + *pc++);
      "$IMM") a=$imm ;;                         # a = *pc++;
      "$REF") a=$imm ;;                         # a = *pc++;
      "$JMP") pc=$imm ;;                        # pc = (int *)*pc;
      "$JSR") push_stack $pc ; pc=$imm ;;       # { *--sp = (int)(pc + 1); pc = (int *)*pc; }
      "$BZ") [ $a -eq 0 ] && pc=$imm ;;         # pc = a ? pc + 1 : (int *)*pc;
      "$BNZ") [ $a -ne 0 ] && pc=$imm ;;       # pc = a ? (int *)*pc : pc + 1;
      "$ENT")                                   # { *--sp = (int)bp; bp = sp; sp = sp - *pc++; } // enter subroutine
        push_stack $bp
        bp=$sp
        sp=$((sp - imm))
        ;;
      "$ADJ") sp=$((sp + imm)) ;;               # sp += *pc++; // stack adjust
      "$LEV")                                   # { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
        sp=$bp
        bp=$((_data_$sp))
        sp=$((sp + 1))
        pc=$((_data_$sp))
        sp=$((sp + 1))
        ;;
      "$LI") a=$((_data_$a)) ;;                 # a = *(int *)a;
      "$LC") a=$((_data_$a)) ;;                 # a = *(char *)a;
      "$SI") : $((_data_$((_data_$sp))=$a)) ; : $((sp += 1)) ;;  # *(int *)*sp++ = a;
      "$SC") : $((_data_$((_data_$sp))=$a)) ; : $((sp += 1)) ;;  # a = *(char *)*sp++ = a;
      "$PSH") push_stack "$a" ;;                # *--sp = a;
      "$OR")  pop_stack; a=$((res | a)) ;;
      "$XOR") pop_stack; a=$((res ^ a)) ;;
      "$AND") pop_stack; a=$((res & a)) ;;
      "$EQ")  pop_stack; a=$((res == a)) ;;
      "$NE")  pop_stack; a=$((res != a)) ;;
      "$LT")  pop_stack; a=$((res < a)) ;;
      "$GT")  pop_stack; a=$((res > a)) ;;
      "$LE")  pop_stack; a=$((res <= a)) ;;
      "$GE")  pop_stack; a=$((res >= a)) ;;
      "$SHL") pop_stack; a=$((res << a)) ;;
      "$SHR") pop_stack; a=$((res >> a)) ;;
      "$ADD") pop_stack; a=$((res + a)) ;;
      "$SUB") pop_stack; a=$((res - a)) ;;
      "$MUL") pop_stack; a=$((res * a)) ;;
      "$DIV") pop_stack; a=$((res / a)) ;;
      "$MOD") pop_stack; a=$((res % a)) ;;
      "$OPEN")                                  # a = open((char *)sp[1], *sp);
        # We represent file descriptors as strings. That means that modes and offsets do not work.
        # These limitations are acceptable since c4.cc does not use them.
        # TODO: Packing and unpacking the string is a lazy way of copying a string
        at_stack 1
        pack_string "$res"
        unpack_string "$res"
        a=$addr
        ;;
      "$READ")                                  # a = read(sp[2], (char *)sp[1], *sp);
        at_stack 2; fd=$res
        at_stack 1; buf=$res
        at_stack 0; count=$res
        pack_string "$fd"
        read_n_char $count $buf < "$res" # We don't want to use cat because it's not pure Shell
        ;;
      "$CLOS")                                  # a = close(*sp);
        # NOP
        ;;
      "$PRTF")                                  # { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
        # Unlike other primitives, PRTF can get a variable number of arguments
        # and the arity is not encoded in the instruction. Fortunately, all PRTF
        # instructions are followed by an ADJ instruction with the number of
        # arguments to printf as parameter. so we can just read the number of
        # arguments from that instruction (which is 2 words ahead).
        : $((count = _data_$((pc + 1))))
        at_stack $((count - 1)); fmt=$res
        c_printf $count "$fmt"
        ;;
      "$MALC")                                  # a = (int)malloc(*sp);
        # Simple bump allocator, no GC
        mem_to_alloc=$((_data_$sp))
        alloc_memory $mem_to_alloc
        a=$res
        ;;
      "$FREE")                                  # free((void *)*sp);
        # NOP
        # Maybe zero out the memory to make debugging easier?
        ;;
      "$MSET")                                  # a = (int)memset((char *)sp[2], sp[1], *sp);
        at_stack 2; dst=$res
        at_stack 1; val=$res
        at_stack 0; len=$res
        ix=0
        while [ $ix -lt $len ]; do
          : $((_data_$((dst + ix)) = val))
          : $((ix += 1))
        done
        ;;
      "$MCMP")                                  # a = memcmp((char *)sp[2], (char *)sp[1], *sp);
        at_stack 2; op1=$res
        at_stack 1; op2=$res
        at_stack 0; len=$res
        ix=0; a=0
        while [ $ix -lt $len ]; do
          if [ $((_data_$((op1 + ix)))) -ne $((_data_$((op2 + ix)))) ] ; then
            # From man page: returns the difference between the first two differing bytes (treated as unsigned char values
            : $((a = _data_$((op1 + ix)) - _data_$((op2 + ix))))
            break
          fi
          : $((ix = ix + 1))
        done
        ;;
      "$EXIT")                                  # { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
        echo "exit($a) cycle = $cycle"
        exit "$a"
        ;;
      *)
        echo "unknown instruction = $i! cycle = $cycle"
        exit 1
        ;;
    esac
  done
}

run() {
  dat_start=$dat
  read_data
  instr_start=$dat
  get_num
  main_addr=$(($value + $instr_start))
  read_instructions $dat_start $instr_start
  last_instr=$dat
  if [ $debug -eq 1 ] ; then
    print_instructions
  fi

  # sp=0;
  pc=$main_addr; bp=$sp; a=0; cycle=0; # vm registers
  i=0; t=0 # temps

  # setup first stack frame
  push_stack $EXIT # call exit if main returns
  push_stack $PSH
  t=$sp
  argc=$#; push_stack $argc # argc
  alloc_memory $argc ; argv_ptr=$res ; push_stack $res # argv

  while [ $# -ge 1 ]; do
    unpack_string "$1"
    : $((_data_$argv_ptr = $addr))
    : $((argv_ptr += 1))
    shift
  done
  push_stack $t

  # exit 1

  run_instructions
}

show_vm_state() {
  echo "    pc = $pc, sp = $sp, bp = $bp, hp = $dat, a = $a"
}

# Because the stack may contain undefined values, this code is incompatible with the set -u option
show_stack() {
  echo "    Stack:"
  stack_ix=$INITIAL_STACK_POS
  while [ $stack_ix -gt $sp ]; do
    : $((stack_ix -= 1))
    echo "        _data_$stack_ix = $((_data_$stack_ix))"
  done
}

show_heap() {
  heap_ix=$INITIAL_HEAP_POS
  echo "    Heap:"
  while [ $heap_ix -lt $dat ]; do
    ascii=$((_data_$heap_ix))
    char=""
    if [ $ascii -ge 31 ] && [ $ascii -le 127 ] ; then
      char=$(printf "\\$(printf "%o" "$ascii")")
    fi
    echo "        _data_$heap_ix = $ascii  ($char)"
    : $((heap_ix += 1))
  done
}

run $@ < "$1"
