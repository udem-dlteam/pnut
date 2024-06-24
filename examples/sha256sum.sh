set -e -u

# /*
#  * compile with:
#  *
#  *   $ ksh pnut.sh sha256sum.c > sha256sum.sh
#  *
#  * execute with:
#  *
#  *   $ ksh sha256sum.sh FILE
#  */
# int k[64];
__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

alloc() {
  : $((_$__ALLOC = $1)) # Save allocation size
  : $((__ALLOC += 1))
  __addr=$__ALLOC
  : $((__ALLOC += $1))
}

defarr() { alloc $2; : $(( $1 = __addr )); }


defarr _k 64
#################################### C code ####################################
# void sha256_setup() {
#   k[ 0] = 0x428a2f98;
#   k[ 1] = 0x71374491;
#   k[ 2] = 0xb5c0fbcf;
#   k[ 3] = 0xe9b5dba5;
#   k[ 4] = 0x3956c25b;
#   k[ 5] = 0x59f111f1;
#   k[ 6] = 0x923f82a4;
#   k[ 7] = 0xab1c5ed5;
#   k[ 8] = 0xd807aa98;
#   k[ 9] = 0x12835b01;
#   k[10] = 0x243185be;
#   k[11] = 0x550c7dc3;
#   k[12] = 0x72be5d74;
#   k[13] = 0x80deb1fe;
#   k[14] = 0x9bdc06a7;
#   k[15] = 0xc19bf174;
#   k[16] = 0xe49b69c1;
#   k[17] = 0xefbe4786;
#   k[18] = 0x0fc19dc6;
#   k[19] = 0x240ca1cc;
#   k[20] = 0x2de92c6f;
#   k[21] = 0x4a7484aa;
#   k[22] = 0x5cb0a9dc;
#   k[23] = 0x76f988da;
#   k[24] = 0x983e5152;
#   k[25] = 0xa831c66d;
#   k[26] = 0xb00327c8;
#   k[27] = 0xbf597fc7;
#   k[28] = 0xc6e00bf3;
#   k[29] = 0xd5a79147;
#   k[30] = 0x06ca6351;
#   k[31] = 0x14292967;
#   k[32] = 0x27b70a85;
#   k[33] = 0x2e1b2138;
#   k[34] = 0x4d2c6dfc;
#   k[35] = 0x53380d13;
#   k[36] = 0x650a7354;
#   k[37] = 0x766a0abb;
#   k[38] = 0x81c2c92e;
#   k[39] = 0x92722c85;
#   k[40] = 0xa2bfe8a1;
#   k[41] = 0xa81a664b;
#   k[42] = 0xc24b8b70;
#   k[43] = 0xc76c51a3;
#   k[44] = 0xd192e819;
#   k[45] = 0xd6990624;
#   k[46] = 0xf40e3585;
#   k[47] = 0x106aa070;
#   k[48] = 0x19a4c116;
#   k[49] = 0x1e376c08;
#   k[50] = 0x2748774c;
#   k[51] = 0x34b0bcb5;
#   k[52] = 0x391c0cb3;
#   k[53] = 0x4ed8aa4a;
#   k[54] = 0x5b9cca4f;
#   k[55] = 0x682e6ff3;
#   k[56] = 0x748f82ee;
#   k[57] = 0x78a5636f;
#   k[58] = 0x84c87814;
#   k[59] = 0x8cc70208;
#   k[60] = 0x90befffa;
#   k[61] = 0xa4506ceb;
#   k[62] = 0xbef9a3f7;
#   k[63] = 0xc67178f2;
# }
################################# End of C code ################################
_sha256_setup() {
  : $(( _$((_k+0)) = 1116352408 ))
  : $(( _$((_k+1)) = 1899447441 ))
  : $(( _$((_k+2)) = -1245643825 ))
  : $(( _$((_k+3)) = -373957723 ))
  : $(( _$((_k+4)) = 961987163 ))
  : $(( _$((_k+5)) = 1508970993 ))
  : $(( _$((_k+6)) = -1841331548 ))
  : $(( _$((_k+7)) = -1424204075 ))
  : $(( _$((_k+8)) = -670586216 ))
  : $(( _$((_k+9)) = 310598401 ))
  : $(( _$((_k+10)) = 607225278 ))
  : $(( _$((_k+11)) = 1426881987 ))
  : $(( _$((_k+12)) = 1925078388 ))
  : $(( _$((_k+13)) = -2132889090 ))
  : $(( _$((_k+14)) = -1680079193 ))
  : $(( _$((_k+15)) = -1046744716 ))
  : $(( _$((_k+16)) = -459576895 ))
  : $(( _$((_k+17)) = -272742522 ))
  : $(( _$((_k+18)) = 264347078 ))
  : $(( _$((_k+19)) = 604807628 ))
  : $(( _$((_k+20)) = 770255983 ))
  : $(( _$((_k+21)) = 1249150122 ))
  : $(( _$((_k+22)) = 1555081692 ))
  : $(( _$((_k+23)) = 1996064986 ))
  : $(( _$((_k+24)) = -1740746414 ))
  : $(( _$((_k+25)) = -1473132947 ))
  : $(( _$((_k+26)) = -1341970488 ))
  : $(( _$((_k+27)) = -1084653625 ))
  : $(( _$((_k+28)) = -958395405 ))
  : $(( _$((_k+29)) = -710438585 ))
  : $(( _$((_k+30)) = 113926993 ))
  : $(( _$((_k+31)) = 338241895 ))
  : $(( _$((_k+32)) = 666307205 ))
  : $(( _$((_k+33)) = 773529912 ))
  : $(( _$((_k+34)) = 1294757372 ))
  : $(( _$((_k+35)) = 1396182291 ))
  : $(( _$((_k+36)) = 1695183700 ))
  : $(( _$((_k+37)) = 1986661051 ))
  : $(( _$((_k+38)) = -2117940946 ))
  : $(( _$((_k+39)) = -1838011259 ))
  : $(( _$((_k+40)) = -1564481375 ))
  : $(( _$((_k+41)) = -1474664885 ))
  : $(( _$((_k+42)) = -1035236496 ))
  : $(( _$((_k+43)) = -949202525 ))
  : $(( _$((_k+44)) = -778901479 ))
  : $(( _$((_k+45)) = -694614492 ))
  : $(( _$((_k+46)) = -200395387 ))
  : $(( _$((_k+47)) = 275423344 ))
  : $(( _$((_k+48)) = 430227734 ))
  : $(( _$((_k+49)) = 506948616 ))
  : $(( _$((_k+50)) = 659060556 ))
  : $(( _$((_k+51)) = 883997877 ))
  : $(( _$((_k+52)) = 958139571 ))
  : $(( _$((_k+53)) = 1322822218 ))
  : $(( _$((_k+54)) = 1537002063 ))
  : $(( _$((_k+55)) = 1747873779 ))
  : $(( _$((_k+56)) = 1955562222 ))
  : $(( _$((_k+57)) = 2024104815 ))
  : $(( _$((_k+58)) = -2067236844 ))
  : $(( _$((_k+59)) = -1933114872 ))
  : $(( _$((_k+60)) = -1866530822 ))
  : $(( _$((_k+61)) = -1538233109 ))
  : $(( _$((_k+62)) = -1090935817 ))
  : $(( _$((_k+63)) = -965641998 ))
}

# int w[64];
defarr _w 64
# int nbits;
_nbits=0
# int hash[8];
defarr _hash 8
# int temp[8];
defarr _temp 8
#################################### C code ####################################
# void sha256_init() {
#   nbits = 0;
#   hash[0] = 0x6a09e667;
#   hash[1] = 0xbb67ae85;
#   hash[2] = 0x3c6ef372;
#   hash[3] = 0xa54ff53a;
#   hash[4] = 0x510e527f;
#   hash[5] = 0x9b05688c;
#   hash[6] = 0x1f83d9ab;
#   hash[7] = 0x5be0cd19;
# }
################################# End of C code ################################
_sha256_init() {
  _nbits=0
  : $(( _$((_hash+0)) = 1779033703 ))
  : $(( _$((_hash+1)) = -1150833019 ))
  : $(( _$((_hash+2)) = 1013904242 ))
  : $(( _$((_hash+3)) = -1521486534 ))
  : $(( _$((_hash+4)) = 1359893119 ))
  : $(( _$((_hash+5)) = -1694144372 ))
  : $(( _$((_hash+6)) = 528734635 ))
  : $(( _$((_hash+7)) = 1541459225 ))
}

#################################### C code ####################################
# void sha256_add_block(char *bytes) {
#   int b0;
#   int b1;
#   int b2;
#   int b3;
#   int s0;
#   int s1;
#   int i;
#   int ch;
#   int t1;
#   int ma;
#   int t2;
#   for (i=0; i<16; ++i) {
#     b0 = 0xff & bytes[i*4];
#     b1 = 0xff & bytes[i*4+1];
#     b2 = 0xff & bytes[i*4+2];
#     b3 = 0xff & bytes[i*4+3];
#     w[i] = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
#   }
#   for (i=16; i<64; ++i) {
#     s0 = rot32(w[i-15], 7) ^ rot32(w[i-15], 18) ^ ((w[i-15] >> 3) & u29mask);
#     s1 = rot32(w[i-2], 17) ^ rot32(w[i-2], 19) ^ ((w[i-2] >> 10) & u22mask);
#     w[i] = (w[i-16] + s0 + w[i-7] + s1) & u32mask;
#   }
#   for (i=0; i<8; ++i) temp[i] = hash[i];
#   for (i=0; i<64; ++i) {
#     s1 = rot32(temp[4], 6) ^ rot32(temp[4], 11) ^ rot32(temp[4], 25);
#     ch = (temp[4] & temp[5]) ^ (~temp[4] & temp[6]);
#     t1 = (temp[7] + s1 + ch + k[i] + w[i]) & u32mask;
#     s0 = rot32(temp[0], 2) ^ rot32(temp[0], 13) ^ rot32(temp[0], 22);
#     ma = (temp[0] & temp[1]) ^ (temp[0] & temp[2]) ^ (temp[1] & temp[2]);
#     t2 = (s0 + ma) & u32mask;
#     temp[7] = temp[6];
#     temp[6] = temp[5];
#     temp[5] = temp[4];
#     temp[4] = (temp[3] + t1) & u32mask;
#     temp[3] = temp[2];
#     temp[2] = temp[1];
#     temp[1] = temp[0];
#     temp[0] = (t1 + t2) & u32mask;
#   }
#   for (i=0; i<8; ++i) hash[i] = (hash[i] + temp[i]) & u32mask;
# }
################################# End of C code ################################
: $((t2 = ma = t1 = ch = i = s1 = s0 = b3 = b2 = b1 = b0 = bytes = 0))
_sha256_add_block() { # bytes: $2
  let bytes; let b0; let b1; let b2; let b3; let s0; let s1; let i; let ch; let t1; let ma; let t2
  bytes=$2
  b0=0
  b1=0
  b2=0
  b3=0
  s0=0
  s1=0
  i=0
  ch=0
  t1=0
  ma=0
  t2=0
  i=0
  while [ $i -lt 16 ] ; do
    b0=$((255 & _$((bytes+(i * 4)))))
    b1=$((255 & _$((bytes+((i * 4) + 1)))))
    b2=$((255 & _$((bytes+((i * 4) + 2)))))
    b3=$((255 & _$((bytes+((i * 4) + 3)))))
    : $(( _$((_w+i)) = ((((b0 << 24) | (b1 << 16)) | (b2 << 8)) | b3) ))
    : $((i += 1))
  done
  i=16
  while [ $i -lt 64 ] ; do
    s0=$((((((_$((_w+(i - 15))) >> 7) & (2147483647 >> (7 - 1))) | ((_$((_w+(i - 15))) << (32 - 7)) & -1)) ^ (((_$((_w+(i - 15))) >> 18) & (2147483647 >> (18 - 1))) | ((_$((_w+(i - 15))) << (32 - 18)) & -1))) ^ ((_$((_w+(i - 15))) >> 3) & 536870911)))
    s1=$((((((_$((_w+(i - 2))) >> 17) & (2147483647 >> (17 - 1))) | ((_$((_w+(i - 2))) << (32 - 17)) & -1)) ^ (((_$((_w+(i - 2))) >> 19) & (2147483647 >> (19 - 1))) | ((_$((_w+(i - 2))) << (32 - 19)) & -1))) ^ ((_$((_w+(i - 2))) >> 10) & 4194303)))
    : $(( _$((_w+i)) = ((((_$((_w+(i - 16))) + s0) + _$((_w+(i - 7)))) + s1) & -1) ))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 8 ] ; do
    : $(( _$((_temp+i)) = _$((_hash+i)) ))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 64 ] ; do
    s1=$((((((_$((_temp+4)) >> 6) & (2147483647 >> (6 - 1))) | ((_$((_temp+4)) << (32 - 6)) & -1)) ^ (((_$((_temp+4)) >> 11) & (2147483647 >> (11 - 1))) | ((_$((_temp+4)) << (32 - 11)) & -1))) ^ (((_$((_temp+4)) >> 25) & (2147483647 >> (25 - 1))) | ((_$((_temp+4)) << (32 - 25)) & -1))))
    ch=$(((_$((_temp+4)) & _$((_temp+5))) ^ (~(_$((_temp+4))) & _$((_temp+6)))))
    t1=$((((((_$((_temp+7)) + s1) + ch) + _$((_k+i))) + _$((_w+i))) & -1))
    s0=$((((((_$((_temp+0)) >> 2) & (2147483647 >> (2 - 1))) | ((_$((_temp+0)) << (32 - 2)) & -1)) ^ (((_$((_temp+0)) >> 13) & (2147483647 >> (13 - 1))) | ((_$((_temp+0)) << (32 - 13)) & -1))) ^ (((_$((_temp+0)) >> 22) & (2147483647 >> (22 - 1))) | ((_$((_temp+0)) << (32 - 22)) & -1))))
    ma=$((((_$((_temp+0)) & _$((_temp+1))) ^ (_$((_temp+0)) & _$((_temp+2)))) ^ (_$((_temp+1)) & _$((_temp+2)))))
    t2=$(((s0 + ma) & -1))
    : $(( _$((_temp+7)) = _$((_temp+6)) ))
    : $(( _$((_temp+6)) = _$((_temp+5)) ))
    : $(( _$((_temp+5)) = _$((_temp+4)) ))
    : $(( _$((_temp+4)) = ((_$((_temp+3)) + t1) & -1) ))
    : $(( _$((_temp+3)) = _$((_temp+2)) ))
    : $(( _$((_temp+2)) = _$((_temp+1)) ))
    : $(( _$((_temp+1)) = _$((_temp+0)) ))
    : $(( _$((_temp+0)) = ((t1 + t2) & -1) ))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 8 ] ; do
    : $(( _$((_hash+i)) = ((_$((_hash+i)) + _$((_temp+i))) & -1) ))
    : $((i += 1))
  done
  endlet $1 t2 ma t1 ch i s1 s0 b3 b2 b1 b0 bytes
}

# char buf[BLOCK_SIZE];
defarr _buf 64
#################################### C code ####################################
# void hex(int byte) {
#   char *digits = "0123456789abcdef";
#   putchar(digits[0xf & (byte >> 4)]);
#   putchar(digits[0xf & byte]);
# }
################################# End of C code ################################
: $((digits = byte = 0))
_hex() { # byte: $2
  let byte; let digits
  byte=$2
  defstr __str_0 "0123456789abcdef"
  digits=$__str_0
  printf \\$(($((_$((digits+(15 & (byte >> 4))))))/64))$(($((_$((digits+(15 & (byte >> 4))))))/8%8))$(($((_$((digits+(15 & (byte >> 4))))))%8))
  printf \\$(($((_$((digits+(15 & byte)))))/64))$(($((_$((digits+(15 & byte)))))/8%8))$(($((_$((digits+(15 & byte)))))%8))
  endlet $1 digits byte
}

#################################### C code ####################################
# int process_file(char *filename) {
#   int i;
#   int fd;
#   int n = BLOCK_SIZE;
#   int h;
#   sha256_setup();
#   sha256_init();
#   fd = open(filename, 0);
#   while (n == BLOCK_SIZE) {
#     n = read(fd, buf, BLOCK_SIZE);
#     if (n < 0) return 1;
#     nbits += 8*n;
#     if (n < BLOCK_SIZE) {
#       buf[n] = 0x80;
#       for (i=n+1; i<BLOCK_SIZE; ++i) buf[i] = 0;
#       if (n >= BLOCK_SIZE-9) {
#         sha256_add_block(buf);
#         for (i=0; i<BLOCK_SIZE-8; ++i) buf[i] = 0;
#       }
#       for (i=1; i<=8; ++i) {
#         buf[BLOCK_SIZE-i] = 0xff & nbits;
#         nbits >>= 8;
#       }
#     }
#     sha256_add_block(buf);
#   }
#   close(fd);
#   for (i=0; i<8; ++i) {
#     h = hash[i];
#     hex(h >> 24);
#     hex(h >> 16);
#     hex(h >> 8);
#     hex(h);
#   }
#   putchar(' ');
#   putchar(' ');
#   while (*filename) {
#     putchar(*filename);
#     ++filename;
#   }
#   putchar('\n');
#   return 0;
# }
################################# End of C code ################################
: $((h = n = fd = i = filename = 0))
_process_file() { # filename: $2
  let filename; let i; let fd; let n; let h
  filename=$2
  i=0
  fd=0
  n=64
  h=0
  _sha256_setup __ 
  _sha256_init __ 
  _open fd $filename 0
  while [ $n = 64 ] ; do
    _read n $fd $_buf 64
    if [ $n -lt 0 ] ; then
      : $(( $1 = 1 ))
      endlet $1 h n fd i filename
      return
    fi
    : $((_nbits += (8 * n)))
    if [ $n -lt 64 ] ; then
      : $(( _$((_buf+n)) = 128 ))
      i=$((n + 1))
      while [ $i -lt 64 ] ; do
        : $(( _$((_buf+i)) = 0 ))
        : $((i += 1))
      done
      if [ $n -ge $((64 - 9)) ] ; then
        _sha256_add_block __ $_buf
        i=0
        while [ $i -lt $((64 - 8)) ] ; do
          : $(( _$((_buf+i)) = 0 ))
          : $((i += 1))
        done
      fi
      i=1
      while [ $i -le 8 ] ; do
        : $(( _$((_buf+(64 - i))) = (255 & _nbits) ))
        : $((_nbits >>= 8))
        : $((i += 1))
      done
    fi
    _sha256_add_block __ $_buf
  done
  _close __ $fd
  i=0
  while [ $i -lt 8 ] ; do
    h=$((_$((_hash+i))))
    _hex __ $((h >> 24))
    _hex __ $((h >> 16))
    _hex __ $((h >> 8))
    _hex __ $h
    : $((i += 1))
  done
  printf \\$(($__CH_SPACE/64))$(($__CH_SPACE/8%8))$(($__CH_SPACE%8))
  printf \\$(($__CH_SPACE/64))$(($__CH_SPACE/8%8))$(($__CH_SPACE%8))
  while [ $(( _$filename )) -ne 0 ] ; do
    printf \\$(($((_$filename))/64))$(($((_$filename))/8%8))$(($((_$filename))%8))
    : $((filename += 1))
  done
  printf \\$(($__CH_NEWLINE/64))$(($__CH_NEWLINE/8%8))$(($__CH_NEWLINE%8))
  : $(( $1 = 0 ))
  endlet $1 h n fd i filename
}

#################################### C code ####################################
# int main(int argc, char **myargv) {
#   int i;
#   for (i=1; i<argc; ++i) {
#     if (process_file(myargv[i]) != 0) break;
#   }
#   return 0;
# }
################################# End of C code ################################
: $((__g1 = i = myargv = argc = 0))
_main() { # argc: $2, myargv: $3
  let argc; let myargv; let i; let __g1
  argc=$2
  myargv=$3
  i=0
  i=1
  while [ $i -lt $argc ] ; do
    if _process_file __g1 $((_$((myargv+i)))); [ $__g1 != 0 ] ; then
      break
    fi
    : $((i += 1))
  done
  : $(( $1 = 0 ))
  endlet $1 __g1 i myargv argc
}

# Character constants
readonly __CH_NEWLINE=10
readonly __CH_SPACE=32
# Runtime library

unpack_escaped_string() {
  __buf="$1"
  # Allocates enough space for all characters, assuming that no character is escaped
  alloc $(( ${#__buf} + 1 ))
  __ptr=$__addr
  while [ -n "$__buf" ] ; do
    case "$__buf" in
      '\'*)
        __buf="${__buf#?}"               # remove the current char from $__buf
        case "$__buf" in
          'a'*) __c=7 ;;
          'b'*) __c=8 ;;
          'f'*) __c=12 ;;
          'n'*) __c=10 ;;
          'r'*) __c=13 ;;
          't'*) __c=9 ;;
          'v'*) __c=11 ;;
          '\'*) __c=92 ;;
          '"'*) __c=34 ;;
          "'"*) __c=39 ;;
          '?'*) __c=63 ;;
          '$'*) __c=36 ;; # Not in C, used to escape variable expansion between double quotes
          *) echo "invalid escape in string: $__char"; exit 1 ;;
        esac
        __buf="${__buf#?}"               # remove the current char from $__buf
        ;;
      *)
        __c=$(LC_CTYPE=C printf "%d" "'${__buf%"${__buf#?}"}")
        __buf="${__buf#?}"                  # remove the current char from $__buf
        ;;
    esac
    : $((_$__ptr = __c))
    : $((__ptr += 1))
  done
  : $((_$__ptr = 0 ))
}

# Define a string, and return a reference to it in the varible taken as argument.
# If the variable is already defined, this function does nothing.
# Note that it's up to the caller to ensure that no 2 strings share the same variable.
defstr() { # $1 = variable name, $2 = string
  set +u # Necessary to allow the variable to be empty
  if [ $(($1)) -eq 0 ]; then
    unpack_escaped_string "$2"
    : $(( $1 = __addr ))
  fi
  set -u
}

_free() { # $1 = pointer to object to free
  : $(($1 = 0)); shift # Return 0
  __ptr=$1
  __size=$((_$((__ptr - 1)))) # Get size of allocation
  while [ $__size -gt 0 ]; do
    unset "_$__ptr"
    : $((__ptr += 1))
    : $((__size -= 1))
  done
}

# Unpack a Shell string into an appropriately sized buffer
unpack_line() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?
  __fgetc_buf=$1
  __buffer=$2
  __ends_with_eof=$3
  while [ ! -z "$__fgetc_buf" ]; do
    __c=$(LC_CTYPE=C printf "%d" "'${__fgetc_buf%"${__fgetc_buf#?}"}")
    : $((_$__buffer = __c))
    __fgetc_buf=${__fgetc_buf#?}      # Remove the first character
    : $((__buffer += 1))              # Move to the next buffer position
  done

  if [ $__ends_with_eof -eq 0 ]; then # Ends with newline and not EOF?
    : $(( _$__buffer = 10))           # Line ends with newline
    : $((__buffer += 1))
  fi
  : $(( _$__buffer = 0))              # Then \0
}

refill_buffer() { # $1: fd
  __fd=$1
  __buffer=$((__buffer_fd$__fd))

  IFS=
  if read -r __temp_buf <&$__fd ; then  # read next line into $__temp_buf
    __ends_with_eof=0
  else
    __ends_with_eof=1
  fi

  # Check that the buffer is large enough to unpack the line
  __buflen=$((__buflen_fd$__fd - 2)) # Minus 2 to account for newline and \0
  __len=${#__temp_buf}
  if [ $__len -gt $__buflen ]; then
    # Free buffer and reallocate a new one double the line size
    __buflen=$((__len * 2))
    _free __ $__buffer
    alloc $__buflen
    : $((__buffer_fd$__fd = __addr))
    : $((__buflen_fd$__fd = __buflen))
    __buffer=$__addr
  fi
  unpack_line "$__temp_buf" $__buffer $__ends_with_eof
}

read_byte() { # $2: fd
  __fd=$2
  : $((__buffer=__buffer_fd$__fd))
  : $((__cursor=__cursor_fd$__fd))
  # The cursor is at the end of the buffer, we need to read the next line
  if [ $((_$((__buffer + __cursor)))) -eq 0 ]; then
    # Buffer has been read completely, read next line
    refill_buffer $__fd
    __cursor=0 # Reset cursor and reload buffer
    : $((__buffer=__buffer_fd$__fd))
    if [ $((_$((__buffer + __cursor)))) -eq 0 ]; then
      : $(($1 = -1)) # EOF
      return
    fi
  fi
  : $(($1 = _$((__buffer + __cursor))))
  : $((__cursor_fd$__fd = __cursor + 1))  # Increment cursor
}


# Convert a VM string reference to a Shell string.
# $__res is set to the result, and $__len is set to the length of the string.
pack_string() {
  __addr=$1; shift
  __max_len=100000000
  __delim=0
  __len=0
  __res=""
  if [ $# -ge 1 ] ; then __delim=$1   ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then __max_len=$1 ; shift ; fi # Optional max length
  while [ $((_$__addr)) -ne $__delim ] && [ $__max_len -gt $__len ] ; do
    __char=$((_$__addr))
    __addr=$((__addr + 1))
    __len=$((__len + 1))
    case $__char in
      10) __res="$__res\n" ;; # 10 == '\n'
      *)        __char=$(printf "\\$(printf "%o" "$__char")")
        __res="$__res$__char" ;;
    esac
  done
}

__state_fd0=0;
alloc 1000                    # Allocate buffer
: $(( _$__addr = 0 ))         # Init buffer to ""
: $((__buffer_fd0 = __addr))  # Save buffer address
: $((__cursor_fd0 = 0))       # Make buffer empty
: $((__buflen_fd0 = 1000))    # Init buffer length
__state_fd1=1
__state_fd2=1
__state_fd3=-1
__state_fd4=-1
__state_fd5=-1
__state_fd6=-1
__state_fd7=-1
__state_fd8=-1
__state_fd9=-1

_open() { # $2: filename, $3: flags, $4: mode
  # Get available fd
  __fd=0
  while [ $__fd -lt 10 ]; do
    if [ $((__state_fd$__fd)) -lt 0 ]; then
      break
    fi
    : $((__fd += 1))
  done
  if [ $__fd -gt 9 ] ; then
    # Some shells don't support fd > 9
    echo "No more file descriptors available" ; exit 1
  else
    # Because the file must be read line-by-line, and string
    # values can't be assigned to dynamic variables, each line
    # is read and then unpacked in the buffer.
    alloc 1000                          # Allocate buffer
    : $(( _$__addr = 0 ))               # Init buffer to ""
    : $(( __buffer_fd$__fd = __addr ))  # Save buffer address
    : $(( __cursor_fd$__fd = 0 ))       # Make buffer empty
    : $(( __buflen_fd$__fd = 1000 ))    # Init buffer length
    : $((__state_fd$__fd = $3))         # Mark the fd as opened
    pack_string $2
    if [ $3 = 0 ] ; then
      case $__fd in
        0) exec 0< $__res ;; 1) exec 1< $__res ;; 2) exec 2< $__res ;;
        3) exec 3< $__res ;; 4) exec 4< $__res ;; 5) exec 5< $__res ;;
        6) exec 6< $__res ;; 7) exec 7< $__res ;; 8) exec 8< $__res ;;
        9) exec 9< $__res ;;
      esac
    elif [ $3 = 1 ] ; then
      case $__fd in
        0) exec 0> $__res ;; 1) exec 1> $__res ;; 2) exec 2> $__res ;;
        3) exec 3> $__res ;; 4) exec 4> $__res ;; 5) exec 5> $__res ;;
        6) exec 6> $__res ;; 7) exec 7> $__res ;; 8) exec 8> $__res ;;
        9) exec 9> $__res ;;
      esac
    elif [ $3 = 2 ] ; then
      case $__fd in
        0) exec 0>> $__res ;; 1) exec 1>> $__res ;; 2) exec 2>> $__res ;;
        3) exec 3>> $__res ;; 4) exec 4>> $__res ;; 5) exec 5>> $__res ;;
        6) exec 6>> $__res ;; 7) exec 7>> $__res ;; 8) exec 8>> $__res ;;
        9) exec 9>> $__res ;;
      esac
    else
      echo "Unknow file mode" ; exit 1
    fi
  fi
  : $(($1 = __fd))
}

_read() { : $((__fd = $2, __buf = $3, __count = $4))
  : $((__i = 0))
  while [ $__i -lt $__count ] ; do
    read_byte __byte $__fd
    if [ $__byte -lt 0 ] ; then
      break
    fi
    : $((_$((__buf + __i)) = __byte))
    : $((__i += 1))
  done
  : $(($1 = __i))
}

_close() { # $2: fd
  __fd=$2
  __buf=$((__buffer_fd$__fd))  # Get buffer
  _free __ $__buf              # Release buffer
  : $((__state_fd$__fd = -1))  # Mark the fd as closed
  case $__fd in
    0) exec 0<&- ;; 1) exec 1<&- ;; 2) exec 2<&- ;;
    3) exec 3<&- ;; 4) exec 4<&- ;; 5) exec 5<&- ;;
    6) exec 6<&- ;; 7) exec 7<&- ;; 8) exec 8<&- ;;
    9) exec 9<&- ;;
  esac
  : $(($1 = 0))
}

# Push a Shell string to the VM heap. Returns a reference to the string in $__addr.
unpack_string() {
  __buf="$1"
  alloc $(( ${#__buf} + 1 ))
  __ptr=$__addr
  while [ -n "$__buf" ] ; do
    __char="${__buf%"${__buf#?}"}"   # remove all but first char
    __buf="${__buf#?}"               # remove the current char from $__buf
    __c=$(LC_CTYPE=C printf "%d" "'$__char")
    : $((_$__ptr = __c))
    : $((__ptr += 1))
  done
  : $((_$__ptr = 0 ))
}

make_argv() {
  __argc=$1; shift;
  alloc $__argc # Allocate enough space for all elements. No need to initialize.
  __argv=$__addr # Saving address because its overwritten by unpack_string
  __argv_ptr=$__addr # __ptr is used by unpack_string

  while [ $# -ge 1 ]; do
    unpack_string "$1"
    : $((_$__argv_ptr = $__addr))
    : $((__argv_ptr += 1))
    shift
  done
}

# Local variables
__=0
__SP=0
let() { : $((__SP += 1)) $((__$__SP=$1)); } 
endlet() {
  __ret=$1; : $((__tmp = $__ret)) # Save return value so it's not overwritten
  while [ $# -ge 2 ]; do : $(($2 = __$__SP)) $((__SP -= 1)); shift; done
  : $(($__ret=__tmp))
}

# Setup argc, argv
__argc_for_main=$(($# + 1))
make_argv $__argc_for_main "$0" $@; __argv_for_main=$__argv
_code=0; # Success exit code
_main _code $__argc_for_main $__argv_for_main; exit $_code

# string_pool_alloc=632 heap_alloc=6191 max_text_alloc=5705 cumul_text_alloc=13058
