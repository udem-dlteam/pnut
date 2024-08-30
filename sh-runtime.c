// Produce the shell runtime library

#ifdef INCLUDE_ALL_RUNTIME
#define DEFAULT_USE 1
#else
#define DEFAULT_USE 0
#endif

#define DEFINE_RUNTIME_FUN(name) \
bool runtime_use_ ## name = DEFAULT_USE; \
bool runtime_ ## name ## _defined = false; \
void runtime_ ## name () { \
RETURN_IF_TRUE(runtime_ ## name ## _defined)
#define END_RUNTIME_FUN(name) putstr("\n"); }
#define DEPENDS_ON(name) runtime_ ## name ();
#define RETURN_IF_TRUE(var) if (var) return; var = true;

#ifdef RT_COMPACT
#define call_char_to_int(prefix, char_var) putstr(prefix "__c=$(printf \"%d\" \"'" char_var "\"); __c=$((__c > 0 ? __c : 256 + __c))\n");
#else
#define call_char_to_int(prefix, char_var) putstr(prefix "char_to_int \"" char_var "\"\n");
#endif

#define call_int_to_char(prefix, int_var) putstr(prefix "__char=$(printf \"\\\\$((" int_var "/64))$((" int_var "/8%8))$((" int_var "%8))\")\n");

// The following cases are ordered by frequency in the C source code and correspond to the letters with more than 1000
// occurrences See analyze-big-c.py to see the frequency of each character in big.c.
// Note that adding cases here speeds up all shells except ksh, so the set of optimized characters should be kept small.
#define extract_first_char_fast(prefix, buf_var, res_var) \
  putstr(prefix "  case \"$" buf_var "\" in\n"); \
  putstr(prefix "    \" \"*) : $((" res_var " = 32))  ;;\n"); \
  putstr(prefix "    \"e\"*) : $((" res_var " = 101)) ;;\n"); \
  putstr(prefix "    \"=\"*) : $((" res_var " = 61))  ;;\n"); \
  putstr(prefix "    \"t\"*) : $((" res_var " = 116)) ;;\n"); \
  putstr(prefix "    \";\"*) : $((" res_var " = 59))  ;;\n"); \
  putstr(prefix "    \"i\"*) : $((" res_var " = 105)) ;;\n"); \
  putstr(prefix "    \")\"*) : $((" res_var " = 41))  ;;\n"); \
  putstr(prefix "    \"(\"*) : $((" res_var " = 40))  ;;\n"); \
  putstr(prefix "    \"n\"*) : $((" res_var " = 110)) ;;\n"); \
  putstr(prefix "    \"s\"*) : $((" res_var " = 115)) ;;\n"); \
  putstr(prefix "    \"l\"*) : $((" res_var " = 108)) ;;\n"); \
  putstr(prefix "    \"+\"*) : $((" res_var " = 43))  ;;\n"); \
  putstr(prefix "    \"p\"*) : $((" res_var " = 112)) ;;\n"); \
  putstr(prefix "    \"a\"*) : $((" res_var " = 97))  ;;\n"); \
  putstr(prefix "    \"r\"*) : $((" res_var " = 114)) ;;\n"); \
  putstr(prefix "    \"f\"*) : $((" res_var " = 102)) ;;\n"); \
  putstr(prefix "    \"d\"*) : $((" res_var " = 100)) ;;\n"); \
  putstr(prefix "    \"*\"*) : $((" res_var " = 42))  ;;\n"); \
  putstr(prefix "    *)\n"); \
  call_char_to_int(prefix "      ", "${" buf_var "%\"${" buf_var "#?}\"}") \
  putstr(prefix "      : $((" res_var " = __c))\n"); \
  putstr(prefix "      ;;\n"); \
  putstr(prefix "  esac\n");

#define extract_first_char_compact(prefix, buf_var, res_var) \
  call_char_to_int(prefix "  ", "${" buf_var "%\"${" buf_var "#?}\"}") \
  putstr(prefix "  : $((" res_var " = __c))\n");

#ifdef RT_COMPACT
#define extract_first_char(prefix, buf_var, res_var) extract_first_char_compact(prefix, buf_var, res_var)
#else
#define extract_first_char(prefix, buf_var, res_var) extract_first_char_fast(prefix, buf_var, res_var)
#endif

#define ANY_STRING_16   "????????????????"
#define ANY_STRING_256  "????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????"

#define extract_line_head(prefix, small_buf, big_buf, pattern, len, when_empty) \
  putstr(prefix "if [ -z \"$" small_buf "\" ]; then\n"); \
  putstr(prefix "  if [ ${#" big_buf "} -ge " len " ]; then\n"); \
  putstr(prefix "    __temp=\"${" big_buf "#" pattern "}\"\n"); \
  putstr(prefix "    " small_buf "=\"${" big_buf "%\"$__temp\"}\"\n"); \
  putstr(prefix "    " big_buf "=\"$__temp\"\n"); \
  putstr(prefix "  else\n"); \
  putstr(prefix "    " small_buf "=\"$" big_buf "\"\n"); \
  putstr(prefix "    " big_buf "=\n"); \
  putstr(when_empty); \
  putstr(prefix "  fi\n"); \
  putstr(prefix "fi\n");

// Local variables

DEFINE_RUNTIME_FUN(local_vars)
  putstr("# Local variables\n");
  putstr("__=0\n");
#ifndef SH_SAVE_VARS_WITH_SET
  putstr("__SP=0\n");
#ifdef SH_INITIALIZE_PARAMS_WITH_LET
  putstr("let() { # $1: variable name, $2: value (optional) \n");
  putstr("  : $((__SP += 1)) $((__$__SP=$1)) # Push\n");
  putstr("  : $(($1=${2-0}))                 # Init\n");
  putstr("}\n");
#else
  putstr("let() { : $((__SP += 1)) $((__$__SP=$1)); }\n");
#endif
  putstr("endlet() { # $1: return variable\n");
  putstr("           # $2...: function local variables\n");
  putstr("  __ret=$1 # Don't overwrite return value\n");
  putstr("  : $((__tmp = $__ret))\n");
  putstr("  while [ $# -ge 2 ]; do\n");
  putstr("    : $(($2 = __$__SP)) $((__SP -= 1)); # Pop\n");
  putstr("    shift;\n");
  putstr("  done\n");
  putstr("  : $(($__ret=__tmp))   # Restore return value\n");
  putstr("}\n");
#endif
END_RUNTIME_FUN(local_vars)

// char<->int conversion

DEFINE_RUNTIME_FUN(char_to_int)
#ifndef RT_COMPACT
#ifdef RT_USE_LOOKUP_TABLE
  putstr("__c2i_0=48\n");
  putstr("__c2i_1=49\n");
  putstr("__c2i_2=50\n");
  putstr("__c2i_3=51\n");
  putstr("__c2i_4=52\n");
  putstr("__c2i_5=53\n");
  putstr("__c2i_6=54\n");
  putstr("__c2i_7=55\n");
  putstr("__c2i_8=56\n");
  putstr("__c2i_9=57\n");
  putstr("__c2i_a=97\n");
  putstr("__c2i_b=98\n");
  putstr("__c2i_c=99\n");
  putstr("__c2i_d=100\n");
  putstr("__c2i_e=101\n");
  putstr("__c2i_f=102\n");
  putstr("__c2i_g=103\n");
  putstr("__c2i_h=104\n");
  putstr("__c2i_i=105\n");
  putstr("__c2i_j=106\n");
  putstr("__c2i_k=107\n");
  putstr("__c2i_l=108\n");
  putstr("__c2i_m=109\n");
  putstr("__c2i_n=110\n");
  putstr("__c2i_o=111\n");
  putstr("__c2i_p=112\n");
  putstr("__c2i_q=113\n");
  putstr("__c2i_r=114\n");
  putstr("__c2i_s=115\n");
  putstr("__c2i_t=116\n");
  putstr("__c2i_u=117\n");
  putstr("__c2i_v=118\n");
  putstr("__c2i_w=119\n");
  putstr("__c2i_x=120\n");
  putstr("__c2i_y=121\n");
  putstr("__c2i_z=122\n");
  putstr("__c2i_A=65\n");
  putstr("__c2i_B=66\n");
  putstr("__c2i_C=67\n");
  putstr("__c2i_D=68\n");
  putstr("__c2i_E=69\n");
  putstr("__c2i_F=70\n");
  putstr("__c2i_G=71\n");
  putstr("__c2i_H=72\n");
  putstr("__c2i_I=73\n");
  putstr("__c2i_J=74\n");
  putstr("__c2i_K=75\n");
  putstr("__c2i_L=76\n");
  putstr("__c2i_M=77\n");
  putstr("__c2i_N=78\n");
  putstr("__c2i_O=79\n");
  putstr("__c2i_P=80\n");
  putstr("__c2i_Q=81\n");
  putstr("__c2i_R=82\n");
  putstr("__c2i_S=83\n");
  putstr("__c2i_T=84\n");
  putstr("__c2i_U=85\n");
  putstr("__c2i_V=86\n");
  putstr("__c2i_W=87\n");
  putstr("__c2i_X=88\n");
  putstr("__c2i_Y=89\n");
  putstr("__c2i_Z=90\n");

  putstr("char_to_int() {\n");
  putstr("  case $1 in\n");
  putstr("    [[:alnum:]]) __c=$((__c2i_$1)) ;;\n");
#else
  putstr("char_to_int() {\n");
  putstr("  case $1 in\n");
  putstr("    [0-9]) __c=$((48 + $1)) ;;\n");
  putstr("    'a') __c=97 ;;\n");
  putstr("    'b') __c=98 ;;\n");
  putstr("    'c') __c=99 ;;\n");
  putstr("    'd') __c=100 ;;\n");
  putstr("    'e') __c=101 ;;\n");
  putstr("    'f') __c=102 ;;\n");
  putstr("    'g') __c=103 ;;\n");
  putstr("    'h') __c=104 ;;\n");
  putstr("    'i') __c=105 ;;\n");
  putstr("    'j') __c=106 ;;\n");
  putstr("    'k') __c=107 ;;\n");
  putstr("    'l') __c=108 ;;\n");
  putstr("    'm') __c=109 ;;\n");
  putstr("    'n') __c=110 ;;\n");
  putstr("    'o') __c=111 ;;\n");
  putstr("    'p') __c=112 ;;\n");
  putstr("    'q') __c=113 ;;\n");
  putstr("    'r') __c=114 ;;\n");
  putstr("    's') __c=115 ;;\n");
  putstr("    't') __c=116 ;;\n");
  putstr("    'u') __c=117 ;;\n");
  putstr("    'v') __c=118 ;;\n");
  putstr("    'w') __c=119 ;;\n");
  putstr("    'x') __c=120 ;;\n");
  putstr("    'y') __c=121 ;;\n");
  putstr("    'z') __c=122 ;;\n");
  putstr("    'A') __c=65 ;;\n");
  putstr("    'B') __c=66 ;;\n");
  putstr("    'C') __c=67 ;;\n");
  putstr("    'D') __c=68 ;;\n");
  putstr("    'E') __c=69 ;;\n");
  putstr("    'F') __c=70 ;;\n");
  putstr("    'G') __c=71 ;;\n");
  putstr("    'H') __c=72 ;;\n");
  putstr("    'I') __c=73 ;;\n");
  putstr("    'J') __c=74 ;;\n");
  putstr("    'K') __c=75 ;;\n");
  putstr("    'L') __c=76 ;;\n");
  putstr("    'M') __c=77 ;;\n");
  putstr("    'N') __c=78 ;;\n");
  putstr("    'O') __c=79 ;;\n");
  putstr("    'P') __c=80 ;;\n");
  putstr("    'Q') __c=81 ;;\n");
  putstr("    'R') __c=82 ;;\n");
  putstr("    'S') __c=83 ;;\n");
  putstr("    'T') __c=84 ;;\n");
  putstr("    'U') __c=85 ;;\n");
  putstr("    'V') __c=86 ;;\n");
  putstr("    'W') __c=87 ;;\n");
  putstr("    'X') __c=88 ;;\n");
  putstr("    'Y') __c=89 ;;\n");
  putstr("    'Z') __c=90 ;;\n");
#endif
  putstr("    ' ') __c=32 ;;\n");
  putstr("    '!') __c=33 ;;\n");
  putstr("    '\"') __c=34 ;;\n");
  putstr("    '#') __c=35 ;;\n");
  putstr("    '$') __c=36 ;;\n");
  putstr("    '%') __c=37 ;;\n");
  putstr("    '&') __c=38 ;;\n");
  putstr("    \"'\") __c=39 ;;\n");
  putstr("    '(') __c=40 ;;\n");
  putstr("    ')') __c=41 ;;\n");
  putstr("    '*') __c=42 ;;\n");
  putstr("    '+') __c=43 ;;\n");
  putstr("    ',') __c=44 ;;\n");
  putstr("    '-') __c=45 ;;\n");
  putstr("    '.') __c=46 ;;\n");
  putstr("    '/') __c=47 ;;\n");
  putstr("    ':') __c=58 ;;\n");
  putstr("    ';') __c=59 ;;\n");
  putstr("    '<') __c=60 ;;\n");
  putstr("    '=') __c=61 ;;\n");
  putstr("    '>') __c=62 ;;\n");
  putstr("    '?') __c=63 ;;\n");
  putstr("    '@') __c=64 ;;\n");
  putstr("    '[') __c=91 ;;\n");
  putstr("    '\\') __c=92 ;;\n");
  putstr("    ']') __c=93 ;;\n");
  putstr("    '^') __c=94 ;;\n");
  putstr("    '_') __c=95 ;;\n");
  putstr("    '`') __c=96 ;;\n");
  putstr("    '{') __c=123 ;;\n");
  putstr("    '|') __c=124 ;;\n");
  putstr("    '}') __c=125 ;;\n");
  putstr("    '~') __c=126 ;;\n");
  putstr("    *)\n");
  putstr("      __c=$(printf \"%d\" \"'$1\"); __c=$((__c > 0 ? __c : 256 + __c)) ;; \n");
  putstr("  esac\n");
  putstr("}\n");
#endif
END_RUNTIME_FUN(char_to_int)

// memory allocation
DEFINE_RUNTIME_FUN(malloc)
  putstr("__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.\n\n");
  putstr("_malloc() { # $2 = object size\n");
#ifdef RT_FREE_UNSETS_VARS
  // When free isn't a no-op, we need to tag all objects with their size
  putstr("  : $((_$__ALLOC = $2)) # Track object size\n");
  putstr("  : $(($1 = $__ALLOC + 1))\n");
  putstr("  : $((__ALLOC += $2 + 1))\n");
#else
  putstr("  : $(($1 = $__ALLOC))\n");
  putstr("  : $((__ALLOC += $2))\n");
#endif
  putstr("}\n");
END_RUNTIME_FUN(malloc)

DEFINE_RUNTIME_FUN(initialize_memory)
  putstr("# Initialize the memory to 0\n");
  putstr("initialize_memory() { # $1 = address, $2 = length\n");
  putstr("  __ix=$1\n");
  putstr("  __last=$(($1 + $2))\n");
  putstr("  while [ $__ix -lt $__last ]; do\n");
  putstr("    : $((_$__ix=0))\n");
  putstr("    : $((__ix += 1))\n");
  putstr("  done\n");
  putstr("}\n");
END_RUNTIME_FUN(initialize_memory)

DEFINE_RUNTIME_FUN(defarr)
DEPENDS_ON(malloc)
#ifdef RT_NO_INIT_GLOBALS
  putstr("defarr() { _malloc $1 $2; }\n");
#else
DEPENDS_ON(initialize_memory)
  printf("defarr() { _malloc $1 $2; initialize_memory $(($1)) $2; }\n");
#endif
END_RUNTIME_FUN(defarr)

#ifdef SUPPORT_ADDRESS_OF_OP
DEFINE_RUNTIME_FUN(defglo)
DEPENDS_ON(malloc)
  putstr("defglo() { _malloc $1 1 ; }\n");
END_RUNTIME_FUN(defglo)
#endif

DEFINE_RUNTIME_FUN(free)
  putstr("_free() { # $2 = object to free\n");
#ifdef RT_FREE_UNSETS_VARS
  putstr("  __ptr=$(($2 - 1))          # Start of object\n");
  putstr("  __end=$((__ptr + _$__ptr)) # End of object\n");
  putstr("  while [ $__ptr -lt $__end ]; do\n");
  putstr("    unset \"_$__ptr\"\n");
  putstr("    : $((__ptr += 1))\n");
  putstr("  done\n");
#endif
  putstr("  : $(($1 = 0))              # Return 0\n");
  putstr("}\n");
END_RUNTIME_FUN(free)

// string packing/unpacking
DEFINE_RUNTIME_FUN(unpack_string)
DEPENDS_ON(malloc)
DEPENDS_ON(char_to_int)
  putstr("# Convert a Shell string to a C string\n");
  putstr("unpack_string() {\n");
  putstr("  __str=\"$2\"\n");
  putstr("  _malloc $1 $((${#__str} + 1))\n");
  putstr("  __ptr=$(($1))\n");
#ifdef OPTIMIZE_LONG_LINES
  putstr("  __us_buf16=\n");
  putstr("  __us_buf256=\n");
  putstr("  while [ ! -z \"$__str\" ] || [ ! -z \"$__us_buf256\" ] ; do\n");
  extract_line_head("  ", "__us_buf256", "__str", ANY_STRING_256, "256", "")
  extract_line_head("  ", "__us_buf16", "__us_buf256", ANY_STRING_16, "16", "")
  putstr("    while [ ! -z \"$__us_buf16\" ]; do\n");
  extract_first_char("    ", "__us_buf16", "_$__ptr")
  putstr("      __us_buf16=${__us_buf16#?}  # Remove the first character\n");
  putstr("      : $((__ptr += 1))           # Move to the next buffer position\n");
  putstr("    done\n");
  putstr("  done\n");
#else
  putstr("  while [ -n \"$__str\" ] ; do\n");
  putstr("    # Remove first char from string\n");
  putstr("    __tail=\"${__str#?}\"\n");
  putstr("    # Remove all but first char\n");
  putstr("    __char=\"${__str%\"$__tail\"}\"\n");
  putstr("    # Convert char to ASCII\n");
  call_char_to_int("    ", "$__char")
  putstr("    # Write character to memory\n");
  putstr("    : $((_$__ptr = __c))\n");
  putstr("    # Continue with rest of string\n");
  putstr("    : $((__ptr += 1))\n");
  putstr("    __str=\"$__tail\"\n");
  putstr("  done\n");
#endif
  putstr("  : $((_$__ptr = 0))\n");
  putstr("}\n");
END_RUNTIME_FUN(unpack_string)

DEFINE_RUNTIME_FUN(make_argv)
DEPENDS_ON(malloc)
DEPENDS_ON(unpack_string)
  putstr("make_argv() {\n");
  putstr("  __argc=$1; shift;\n");
  putstr("  _malloc __argv $__argc # Allocate enough space for all elements. No need to initialize.\n");
  putstr("  __argv_ptr=$__argv\n");
  putstr("\n");
  putstr("  while [ $# -ge 1 ]; do\n");
  putstr("    unpack_string _$__argv_ptr \"$1\"\n");
  putstr("    : $((__argv_ptr += 1))\n");
  putstr("    shift\n");
  putstr("  done\n");
  putstr("}\n");
END_RUNTIME_FUN(make_argv)

#define handle_escaped_chars(prefix, buf_var, res_var) \
  putstr(prefix "case \"$" buf_var "\" in\n"); \
  putstr(prefix "  '\\'*)\n"); \
  putstr(prefix "    " buf_var "=\"${" buf_var "#?}\" # Remove the current char from $" buf_var "\n"); \
  putstr(prefix "    case \"$" buf_var "\" in\n"); \
  putstr(prefix "      'a'*) " res_var "=7 ;;\n"); \
  putstr(prefix "      'b'*) " res_var "=8 ;;\n"); \
  putstr(prefix "      'f'*) " res_var "=12 ;;\n"); \
  putstr(prefix "      'n'*) " res_var "=10 ;;\n"); \
  putstr(prefix "      'r'*) " res_var "=13 ;;\n"); \
  putstr(prefix "      't'*) " res_var "=9 ;;\n"); \
  putstr(prefix "      'v'*) " res_var "=11 ;;\n"); \
  putstr(prefix "      '\\'*) " res_var "=92 ;;\n"); \
  putstr(prefix "      '\"'*) " res_var "=34 ;;\n"); \
  putstr(prefix "      \"'\"*) " res_var "=39 ;;\n"); \
  putstr(prefix "      '?'*) " res_var "=63 ;;\n"); \
  putstr(prefix "      '$'*) " res_var "=36 ;; # Not in C, used to escape variable expansion between double quotes\n"); \
  putstr(prefix "      *) echo \"invalid escape in string: $" buf_var "\"; exit 1 ;;\n"); \
  putstr(prefix "    esac\n"); \
  putstr(prefix "    " buf_var "=\"${" buf_var "#?}\" # Remove the current char from $" buf_var "\n"); \
  putstr(prefix "    ;;\n"); \
  putstr(prefix "  *)\n"); \
  call_char_to_int(prefix "    ", "${" buf_var "%\"${" buf_var "#?}\"}") \
  putstr(prefix "    " buf_var "=\"${" buf_var "#?}\" # Remove the current char from $" buf_var "\n"); \
  putstr(prefix "    ;;\n"); \
  putstr(prefix "esac\n");

// See https://en.wikipedia.org/wiki/Escape_sequences_in_C#Table_of_escape_sequences
DEFINE_RUNTIME_FUN(unpack_escaped_string)
DEPENDS_ON(malloc)
DEPENDS_ON(char_to_int)
  putstr("unpack_escaped_string() {\n");
  putstr("  __buf=\"$1\"\n");
  putstr("  # Allocates enough space for all characters, assuming that no character is escaped\n");
  putstr("  _malloc __addr $((${#__buf} + 1))\n");
  putstr("  __ptr=$__addr\n");
#ifdef OPTIMIZE_LONG_LINES
  putstr("  __us_buf16=\n");
  putstr("  __us_buf256=\n");
  putstr("  while [ ! -z \"$__buf\" ] || [ ! -z \"$__us_buf256\" ] ; do\n");
  extract_line_head("    ", "__us_buf256", "__buf", ANY_STRING_256, "256", "")
  extract_line_head("    ", "__us_buf16", "__us_buf256", ANY_STRING_16, "16", "")
  putstr("    while [ ! -z \"$__us_buf16\" ]; do\n");
  handle_escaped_chars("      ", "__us_buf16", "__c")
  putstr("    : $((_$__ptr = __c))\n");
  putstr("    : $((__ptr += 1))\n");
  putstr("    done\n");
  putstr("  done\n");
#else
  putstr("  while [ -n \"$__buf\" ] ; do\n");
  handle_escaped_chars("    ", "__buf", "__c")
  putstr("    : $((_$__ptr = __c))\n");
  putstr("    : $((__ptr += 1))\n");
  putstr("  done\n");
#endif
  putstr("  : $((_$__ptr = 0))\n");
  putstr("}\n");
END_RUNTIME_FUN(unpack_escaped_string)

DEFINE_RUNTIME_FUN(pack_string)
  putstr("# Convert a pointer to a C string to a Shell string.\n");
  putstr("# $__res is set to the result, and $__len is set to the length of the string.\n");
  putstr("pack_string() { # $1 = string address, $2 = end of string delimiter (default to null), $3 = max length (default to 100000000) \n");
  putstr("  __addr=$1; \n");
  putstr("  __max_len=100000000\n");
  putstr("  __delim=0\n");
  putstr("  __len=0\n");
  putstr("  __res=\"\"\n");
  putstr("  if [ $# -ge 2 ] ; then __delim=$2   ; fi # Optional end of string delimiter\n");
  putstr("  if [ $# -ge 3 ] ; then __max_len=$3 ; fi # Optional max length\n");
  putstr("  while [ $((_$__addr)) != $__delim ] && [ $__max_len -gt $__len ] ; do\n");
  putstr("    __char=$((_$__addr))\n");
  putstr("    __addr=$((__addr + 1))\n");
  putstr("    __len=$((__len + 1))\n");
  putstr("    case $__char in\n");
  putstr("      10) __res=\"$__res\\n\" ;; # 10 == '\\n'\n");
  putstr("      *)");
  call_int_to_char("        ", "$__char")
  putstr("        __res=\"$__res$__char\" ;;\n");
  putstr("    esac\n");
  putstr("  done\n");
  putstr("}\n");
END_RUNTIME_FUN(pack_string)

DEFINE_RUNTIME_FUN(defstr)
DEPENDS_ON(unpack_escaped_string)
  putstr("# Define a string, and return a reference to it in the varible taken as argument.\n");
  putstr("# If the variable is already defined, this function does nothing.\n");
  putstr("# Note that it's up to the caller to ensure that no 2 strings share the same variable.\n");
  putstr("defstr() { # $1 = variable name, $2 = string\n");
  putstr("  set +u # Necessary to allow the variable to be empty\n");
  putstr("  if [ $(($1)) -eq 0 ]; then\n");
  putstr("    unpack_escaped_string \"$2\"\n");
  putstr("    : $(($1 = __addr))\n");
  putstr("  fi\n");
  putstr("  set -u\n");
  putstr("}\n");
END_RUNTIME_FUN(defstr)

DEFINE_RUNTIME_FUN(exit)
  putstr("_exit() { # $2: exit status\n");
  putstr("  exit $2\n");
  putstr("}\n");
END_RUNTIME_FUN(exit)

// Input / output
DEFINE_RUNTIME_FUN(putchar)
  putstr("_putchar() {\n");
  putstr("  : $(($1 = 0)); shift # Return 0\n");
  putstr("  printf \\\\$(($1/64))$(($1/8%8))$(($1%8))\n");
  putstr("}\n");
END_RUNTIME_FUN(putchar)

DEFINE_RUNTIME_FUN(getchar)
DEPENDS_ON(char_to_int)
  putstr("__stdin_buf=\n");
  putstr("__stdin_line_ending=0 # Line ending, either -1 (EOF) or 10 ('\\n')\n");
#ifdef OPTIMIZE_LONG_LINES
  putstr("__stdin_buf16=\n");
  putstr("__stdin_buf256=\n");
  putstr("__stdin_end=1\n");
  putstr("_getchar() {\n");
  putstr("  if [ -z \"$__stdin_buf16\" ] && [ $__stdin_end -eq 1 ] ; then          # need to get next line when buffer empty\n");
#else
  putstr("_getchar() {\n");
  putstr("  if [ -z \"$__stdin_buf\" ]; then          # need to get next line when buffer empty\n");
#endif
  putstr("    if [ $__stdin_line_ending != 0 ]; then  # Line is empty, return line ending\n");
  putstr("      : $(($1 = __stdin_line_ending))\n");
  putstr("      __stdin_line_ending=0                  # Reset line ending for next getchar call\n");
  putstr("      return\n");
  putstr("    fi\n");
#ifdef OPTIMIZE_LONG_LINES
  putstr("    __stdin_end=0\n");
#endif
  putstr("    IFS=                                            # don't split input\n");
  putstr("    if read -r __stdin_buf ; then                   # read next line into $__stdin_buf\n");
  putstr("      if [ -z \"$__stdin_buf\" ] ; then               # an empty line implies a newline character\n");
  putstr("        : $(($1 = 10))                              # next getchar call will read next line\n");
  putstr("        return\n");
  putstr("      fi\n");
  putstr("      __stdin_line_ending=10\n");
  putstr("    else\n");
  putstr("      if [ -z \"$__stdin_buf\" ] ; then               # EOF reached when read fails\n");
  putstr("        : $(($1 = -1))\n");
  putstr("        return\n");
  putstr("      else\n");
  putstr("        __stdin_line_ending=-1\n");
  putstr("      fi\n");
  putstr("    fi\n");
  putstr("  fi\n");
#ifdef OPTIMIZE_LONG_LINES
  extract_line_head("  ", "__stdin_buf256", "__stdin_buf", ANY_STRING_256, "256", "")
  extract_line_head("  ", "__stdin_buf16", "__stdin_buf256", ANY_STRING_16, "16", "      __stdin_end=1\n")
  extract_first_char("", "__stdin_buf16", "$1")
  putstr("  __stdin_buf16=${__stdin_buf16#?}  # Remove the first character\n");
#else
  extract_first_char("", "__stdin_buf", "$1")
  putstr("    __stdin_buf=\"${__stdin_buf#?}\"                  # remove the current char from $__stdin_buf\n");
#endif
  putstr("}\n");
END_RUNTIME_FUN(getchar)

// An implementation of puts, used to replace printf("%s", ...) calls.
DEFINE_RUNTIME_FUN(put_pstr)
#ifndef RT_INLINE_PUTCHAR
DEPENDS_ON(putchar)
#endif
  putstr("_put_pstr() {\n");
  putstr("  : $(($1 = 0)); shift # Return 0\n");
  putstr("  __addr=$1; shift\n");
  putstr("  while [ $((_$__addr)) != 0 ]; do\n");
#ifdef RT_INLINE_PUTCHAR
  putstr("    printf \\\\$((_$__addr/64))$((_$__addr/8%8))$((_$__addr%8))\n");
#else
  putstr("    _putchar __ $((_$__addr))\n");
#endif
  putstr("    : $((__addr += 1))\n");
  putstr("  done\n");
  putstr("}\n");
END_RUNTIME_FUN(print_pnut_str)

DEFINE_RUNTIME_FUN(print_string)
  putstr("# Emit a C-string line by line so that whitespace isn't mangled\n");
  putstr("print_string() {\n");
  putstr("  __addr=$1; shift\n");
  putstr("  __max_len=100000000\n");
  putstr("  __delim=0\n");
  putstr("  __len=0\n");
  putstr("  __acc=\"\"\n");
  putstr("  if [ $# -ge 1 ] ; then __delim=$1   ; shift ; fi # Optional end of string delimiter\n");
  putstr("  if [ $# -ge 1 ] ; then __max_len=$1 ; shift ; fi # Optional max length\n");
  putstr("  while [ $((_$__addr)) != $__delim ] && [ $__max_len -gt $__len ] ; do\n");
  putstr("    __char=$((_$__addr))\n");
  putstr("    __addr=$((__addr + 1))\n");
  putstr("    __len=$((__len + 1))\n");
  putstr("    case $__char in\n");
  putstr("      10) # 10 == '\\n'\n");
  putstr("        printf \"%s\\n\" \"$__acc\"\n");
  putstr("        __acc=\"\" ;;\n");
  putstr("      *)\n");
  call_int_to_char("        ", "$__char")
  putstr("        __acc=\"$__acc$__char\" ;;\n");
  putstr("    esac\n");
  putstr("  done\n");
  putstr("  printf \"%s\" \"$__acc\"\n");
  putstr("}\n");
END_RUNTIME_FUN(print_string)

DEFINE_RUNTIME_FUN(printf)
DEPENDS_ON(print_string)
DEPENDS_ON(pack_string)
  putstr("_printf() { # $1 = printf format string, $2... = printf args\n");
  putstr("  : $(($1 = 0)); shift # Return 0\n");
  putstr("  __fmt_ptr=$1; shift\n");
  putstr("  __mod=0\n");
  putstr("  while [ \"$((_$__fmt_ptr))\" != 0 ] ; do\n");
  putstr("    __head=$((_$__fmt_ptr))\n");
  putstr("    __fmt_ptr=$((__fmt_ptr + 1))\n");
  putstr("    if [ $__mod -eq 1 ] ; then\n");
  call_int_to_char("      ", "$__head")
  putstr("      __head_char=$__char\n");
  putstr("      case $__head_char in\n");
  putstr("        'd') # 100 = 'd' Decimal integer\n");
  putstr("          printf \"%d\" $1\n");
  putstr("          shift\n");
  putstr("          ;;\n");
  putstr("        'c') # 99 = 'c' Character\n");
  putstr("          # Don't need to handle non-printable characters the only use of %c is for printable characters\n");
  putstr("          printf \\\\$(($1/64))$(($1/8%8))$(($1%8))\n");
  putstr("          shift\n");
  putstr("          ;;\n");
  putstr("        'x') # 120 = 'x' Hexadecimal integer\n");
  putstr("          printf \"%x\" $1\n");
  putstr("          shift\n");
  putstr("          ;;\n");
  putstr("        's') # 115 = 's' String\n");
  putstr("          print_string $1\n");
  putstr("          shift\n");
  putstr("          ;;\n");
  putstr("        '.') # String with length. %.*s will print the first 4 characters of the string\n");
  putstr("          pack_string $__fmt_ptr 0 2 # Read next 2 characters\n");
  putstr("          __fmt_ptr=$((__fmt_ptr + 2))\n");
  putstr("          if [ \"$__res\" = \"*s\" ]; then\n");
  putstr("            print_string $2 0 $1\n");
  putstr("            shift 2\n");
  putstr("          else\n");
  putstr("            echo \"Unknown format specifier: %.$__res\" ; exit 1\n");
  putstr("          fi\n");
  putstr("          ;;\n");
  putstr("        [0-9])                         # parse integer\n");
  putstr("          # Get max length (with padding)\n");
  putstr("          pack_string $__fmt_ptr 46 # Read until '.' or end of string\n");
  putstr("          __fmt_ptr=$((__fmt_ptr + __len + 1))\n");
  putstr("          __min_len=\"$__head_char$__res\" # Don't forget the first digit we've already read\n");
  putstr("          # Get string length\n");
  putstr("          pack_string $__fmt_ptr 115 # Read until 's' or end of string\n");
  putstr("          __fmt_ptr=$((__fmt_ptr + __len))\n");
  putstr("          __str_len=$__res\n");
  putstr("          __head=$((_$__fmt_ptr))\n");
  call_int_to_char("          ", "$__head")
  putstr("          __head_char=$__char\n");
  putstr("          __fmt_ptr=$((__fmt_ptr + 1))\n");
  putstr("          if [ \"$__head_char\" = 's' ]; then\n");
  putstr("            __str_ref=$1; shift\n");
  putstr("            # Count length of string with pack_string but don't use packed string\n");
  putstr("            pack_string $__str_ref 0 $__str_len\n");
  putstr("            __pad=\"\"\n");
  putstr("            __padlen=$((__min_len - __len)) # Pad string so it has at least $__min_len characters\n");
  putstr("            while [ $__padlen -gt 0 ]; do\n");
  putstr("              __pad=\" $__pad\"\n");
  putstr("              : $((__padlen -= 1))\n");
  putstr("              done\n");
  putstr("            printf \"%s\" \"$__pad\" # Pad string\n");
  putstr("            print_string $__str_ref 0 $__str_len # Print string\n");
  putstr("          else\n");
  putstr("            echo \"Unknown format specifier: '%$__min_len.$__str_len$__head_char'\" ; exit 1;\n");
  putstr("          fi\n");
  putstr("          ;;\n");
  putstr("        *)\n");
  putstr("          echo \"Unknown format specifier %$__head_char\"; exit 1\n");
  putstr("      esac\n");
  putstr("      __mod=0\n");
  putstr("    else\n");
  putstr("      case $__head in\n");
  putstr("        10) printf \"\\n\" ;;  # 10 == '\\n'\n");
  putstr("        37) __mod=1 ;; # 37 == '%'\n");
  putstr("        *) printf \\\\$(($__head/64))$(($__head/8%8))$(($__head%8)) ;;\n");
  putstr("      esac\n");
  putstr("    fi\n");
  putstr("  done\n");
  putstr("}\n");
END_RUNTIME_FUN(printf)

DEFINE_RUNTIME_FUN(open)
DEPENDS_ON(malloc)
DEPENDS_ON(pack_string)
  putstr("__state_fd0=0;\n");
  putstr("_malloc __buffer_fd0 1000   # Allocate buffer\n");
  putstr(": $((_$__buffer_fd0 = 0))   # Init buffer to \"\"\n");
  putstr(": $((__cursor_fd0 = 0))     # Make buffer empty\n");
  putstr(": $((__buflen_fd0 = 1000))  # Init buffer length\n");
  putstr("__state_fd1=1\n");
  putstr("__state_fd2=1\n");
  putstr("__state_fd3=-1\n");
  putstr("__state_fd4=-1\n");
  putstr("__state_fd5=-1\n");
  putstr("__state_fd6=-1\n");
  putstr("__state_fd7=-1\n");
  putstr("__state_fd8=-1\n");
  putstr("__state_fd9=-1\n");
  putstr("\n");
  putstr("_open() { # $2: filename, $3: flags, $4: mode\n");
  putstr("  # Get available fd\n");
  putstr("  __fd=0\n");
  putstr("  while [ $__fd -lt 10 ]; do\n");
  putstr("    if [ $((__state_fd$__fd)) -lt 0 ]; then\n");
  putstr("      break\n");
  putstr("    fi\n");
  putstr("    : $((__fd += 1))\n");
  putstr("  done\n");
  putstr("  if [ $__fd -gt 9 ] ; then\n");
  putstr("    # Some shells don't support fd > 9\n");
  putstr("    echo \"No more file descriptors available\" ; exit 1\n");
  putstr("  else\n");
  putstr("    # Because the file must be read line-by-line, and string\n");
  putstr("    # values can't be assigned to dynamic variables, each line\n");
  putstr("    # is read and then unpacked in the buffer.\n");
  putstr("    _malloc __addr 1000                   # Allocate buffer\n");
  putstr("    : $((_$__addr = 0))                 # Init buffer to \"\"\n");
  putstr("    : $((__buffer_fd$__fd = __addr))    # Save buffer address\n");
  putstr("    : $((__cursor_fd$__fd = 0))         # Make buffer empty\n");
  putstr("    : $((__buflen_fd$__fd = 1000))      # Init buffer length\n");
  putstr("    : $((__state_fd$__fd = $3))         # Mark the fd as opened\n");
  putstr("    pack_string $2\n");
  putstr("    if [ $3 = 0 ] ; then\n");
  putstr("      case $__fd in\n");
  putstr("        0) exec 0< \"$__res\" ;; 1) exec 1< \"$__res\" ;; 2) exec 2< \"$__res\" ;;\n");
  putstr("        3) exec 3< \"$__res\" ;; 4) exec 4< \"$__res\" ;; 5) exec 5< \"$__res\" ;;\n");
  putstr("        6) exec 6< \"$__res\" ;; 7) exec 7< \"$__res\" ;; 8) exec 8< \"$__res\" ;;\n");
  putstr("        9) exec 9< \"$__res\" ;;\n");
  putstr("      esac\n");
  putstr("    elif [ $3 = 1 ] ; then\n");
  putstr("      case $__fd in\n");
  putstr("        0) exec 0> \"$__res\" ;; 1) exec 1> \"$__res\" ;; 2) exec 2> \"$__res\" ;;\n");
  putstr("        3) exec 3> \"$__res\" ;; 4) exec 4> \"$__res\" ;; 5) exec 5> \"$__res\" ;;\n");
  putstr("        6) exec 6> \"$__res\" ;; 7) exec 7> \"$__res\" ;; 8) exec 8> \"$__res\" ;;\n");
  putstr("        9) exec 9> \"$__res\" ;;\n");
  putstr("      esac\n");
  putstr("    elif [ $3 = 2 ] ; then\n");
  putstr("      case $__fd in\n");
  putstr("        0) exec 0>> \"$__res\" ;; 1) exec 1>> \"$__res\" ;; 2) exec 2>> \"$__res\" ;;\n");
  putstr("        3) exec 3>> \"$__res\" ;; 4) exec 4>> \"$__res\" ;; 5) exec 5>> \"$__res\" ;;\n");
  putstr("        6) exec 6>> \"$__res\" ;; 7) exec 7>> \"$__res\" ;; 8) exec 8>> \"$__res\" ;;\n");
  putstr("        9) exec 9>> \"$__res\" ;;\n");
  putstr("      esac\n");
  putstr("    else\n");
  putstr("      echo \"Unknow file mode\" ; exit 1\n");
  putstr("    fi\n");
  putstr("  fi\n");
  putstr("  : $(($1 = __fd))\n");
  putstr("}\n");
END_RUNTIME_FUN(open)

DEFINE_RUNTIME_FUN(read_byte)
DEPENDS_ON(malloc)
DEPENDS_ON(free)
DEPENDS_ON(char_to_int)
  putstr("# Unpack a Shell string into an appropriately sized buffer\n");
  putstr("unpack_line() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?\n");
  putstr("  __fgetc_buf=$1\n");
  putstr("  __buffer=$2\n");
  putstr("  __ends_with_eof=$3\n");
#ifndef OPTIMIZE_LONG_LINES
  putstr("  while [ ! -z \"$__fgetc_buf\" ]; do\n");
  extract_first_char("  ", "__fgetc_buf", "_$__buffer")
  putstr("    __fgetc_buf=${__fgetc_buf#?}      # Remove the first character\n");
  putstr("    : $((__buffer += 1))              # Move to the next buffer position\n");
#else
  putstr("  __fgetc_buf16=\n");
  putstr("  __stdin_buf256=\n");
  putstr("  __continue=1\n");
  putstr("  while [ $__continue != 0 ] ; do\n");
  extract_line_head("    ", "__stdin_buf256", "__fgetc_buf",  ANY_STRING_256, "256", "")
  extract_line_head("    ", "__fgetc_buf16", "__stdin_buf256",  ANY_STRING_16,  "16", "        __continue=0\n")
  putstr("    while [ ! -z \"$__fgetc_buf16\" ]; do\n");
  extract_first_char("    ", "__fgetc_buf16", "_$__buffer")
  putstr("      __fgetc_buf16=${__fgetc_buf16#?}  # Remove the first character\n");
  putstr("      : $((__buffer += 1))              # Move to the next buffer position\n");
  putstr("    done\n");
#endif
  putstr("  done\n");
  putstr("\n");
  putstr("  if [ $__ends_with_eof -eq 0 ]; then # Ends with newline and not EOF?\n");
  putstr("    : $((_$__buffer = 10))            # Line ends with newline\n");
  putstr("    : $((__buffer += 1))\n");
  putstr("  fi\n");
  putstr("  : $((_$__buffer = 0))               # Then \\0\n");
  putstr("}\n");
  putstr("\n");
  putstr("refill_buffer() { # $1: fd\n");
  putstr("  __fd=$1\n");
  putstr("  __buffer=$((__buffer_fd$__fd))\n");
  putstr("\n");
  putstr("  IFS=\n");
  putstr("  if read -r __temp_buf <&$__fd ; then  # read next line into $__temp_buf\n");
  putstr("    __ends_with_eof=0\n");
  putstr("  else\n");
  putstr("    __ends_with_eof=1\n");
  putstr("  fi\n");
  putstr("\n");
  putstr("  # Check that the buffer is large enough to unpack the line\n");
  putstr("  __buflen=$((__buflen_fd$__fd - 2)) # Minus 2 to account for newline and \\0\n");
  putstr("  __len=${#__temp_buf}\n");
  putstr("  if [ $__len -gt $__buflen ]; then\n");
  putstr("    # Free buffer and reallocate a new one double the line size\n");
  putstr("    __buflen=$((__len * 2))\n");
  putstr("    _free __ $__buffer\n");
  putstr("    _malloc __buffer $__buflen\n");
  putstr("    : $((__buffer_fd$__fd = __buffer))\n");
  putstr("    : $((__buflen_fd$__fd = __buflen))\n");
  putstr("  fi\n");
  putstr("  unpack_line \"$__temp_buf\" $__buffer $__ends_with_eof\n");
  putstr("}\n");
  putstr("\n");
  putstr("read_byte() { # $2: fd\n");
  putstr("  __fd=$2\n");
  putstr("  : $((__buffer=__buffer_fd$__fd))\n");
  putstr("  : $((__cursor=__cursor_fd$__fd))\n");
  putstr("  # The cursor is at the end of the buffer, we need to read the next line\n");
  putstr("  if [ $((_$((__buffer + __cursor)))) -eq 0 ]; then\n");
  putstr("    # Buffer has been read completely, read next line\n");
  putstr("    refill_buffer $__fd\n");
  putstr("    __cursor=0 # Reset cursor and reload buffer\n");
  putstr("    : $((__buffer=__buffer_fd$__fd))\n");
  putstr("    if [ $((_$((__buffer + __cursor)))) -eq 0 ]; then\n");
  putstr("      : $(($1 = -1)) # EOF\n");
  putstr("      return\n");
  putstr("    fi\n");
  putstr("  fi\n");
  putstr("  : $(($1 = _$((__buffer + __cursor))))\n");
  putstr("  : $((__cursor_fd$__fd = __cursor + 1))  # Increment cursor\n");
  putstr("}\n");
END_RUNTIME_FUN(read_byte)

DEFINE_RUNTIME_FUN(read)
DEPENDS_ON(read_byte)
DEPENDS_ON(open)
  putstr("_read() { : $((__fd = $2)) $((__buf = $3)) $((__count = $4))\n");
  putstr("  : $((__i = 0))\n");
  putstr("  while [ $__i -lt $__count ] ; do\n");
  putstr("    read_byte __byte $__fd\n");
  putstr("    if [ $__byte -lt 0 ] ; then\n");
  putstr("      break\n");
  putstr("    fi\n");
  putstr("    : $((_$((__buf + __i)) = __byte))\n");
  putstr("    : $((__i += 1))\n");
  putstr("  done\n");
  putstr("  : $(($1 = __i))\n");
  putstr("}\n");
END_RUNTIME_FUN(read)

DEFINE_RUNTIME_FUN(write)
DEPENDS_ON(open)
  putstr("_write() { : $((__fd = $2)) $((__buf = $3)) $((__count = $4))\n");
  putstr("  : $((__i = 0))\n");
  putstr("  while [ $__i -lt $__count ] ; do\n");
  putstr("    : $((__byte = _$((__buf+__i))))\n");
  putstr("    printf \\\\$(($__byte/64))$(($__byte/8%8))$(($__byte%8)) >&$__fd\n");
  putstr("    : $((__i += 1))\n");
  putstr("  done\n");
  putstr("  : $(($1 = __count))\n");
  putstr("}\n");
END_RUNTIME_FUN(write)

// exec $fd<&- does not work as expected so we instead have a case statement
// that calls the appropriate exec command to open and close file descriptors.
DEFINE_RUNTIME_FUN(fopen)
DEPENDS_ON(malloc)
DEPENDS_ON(open)
  putstr("# Open the file and return a FILE* for the file.\n");
  putstr("# The FILE structure contains the file descriptor.\n");
  putstr("_fopen() { # $2: File name, $3: Mode\n");
  putstr("  _open __fd $2 $((_$3 == 119)) 511\n");
  putstr("  _malloc __file 1        # Allocate FILE structure\n");
  putstr("  : $((_$__file = __fd))  # Save fd\n");
  putstr("  : $(($1 = __file))\n");
  putstr("}\n");
END_RUNTIME_FUN(fopen)

DEFINE_RUNTIME_FUN(close)
DEPENDS_ON(open)
DEPENDS_ON(free)
  putstr("_close() { # $2: fd\n");
  putstr("  __fd=$2\n");
  putstr("  __buf=$((__buffer_fd$__fd))  # Get buffer\n");
  putstr("  _free __ $__buf              # Release buffer\n");
  putstr("  : $((__state_fd$__fd = -1))  # Mark the fd as closed\n");
  putstr("  case $__fd in\n");
  putstr("    0) exec 0<&- ;; 1) exec 1<&- ;; 2) exec 2<&- ;;\n");
  putstr("    3) exec 3<&- ;; 4) exec 4<&- ;; 5) exec 5<&- ;;\n");
  putstr("    6) exec 6<&- ;; 7) exec 7<&- ;; 8) exec 8<&- ;;\n");
  putstr("    9) exec 9<&- ;;\n");
  putstr("  esac\n");
  putstr("  : $(($1 = 0))\n");
  putstr("}\n");
END_RUNTIME_FUN(close)

DEFINE_RUNTIME_FUN(fclose)
DEPENDS_ON(free)
DEPENDS_ON(close)
  putstr("_fclose() { # $2: file\n");
  putstr("  __file=$2\n");
  putstr("  __fd=$((_$__file))  # Get fd\n");
  putstr("  _free __ $__file    # Release FILE structure\n");
  putstr("  _close $1 $__fd\n");
  putstr("}\n");
END_RUNTIME_FUN(fclose)

DEFINE_RUNTIME_FUN(fgetc)
DEPENDS_ON(read_byte)
  putstr("_fgetc() { # $2: file\n");
  putstr("  __file=$2\n");
  putstr("  __fd=$((_$__file))\n");
  putstr("  read_byte $1 $__fd\n");
  putstr("}\n");
END_RUNTIME_FUN(fgetc)

void produce_runtime() {
  if (runtime_use_defstr)     runtime_defstr();
  if (runtime_use_putchar)    runtime_putchar();
  if (runtime_use_getchar)    runtime_getchar();
  if (runtime_use_exit)       runtime_exit();
  if (runtime_use_malloc)     runtime_malloc();
  if (runtime_use_free)       runtime_free();
  if (runtime_use_put_pstr)   runtime_put_pstr();
  if (runtime_use_printf)     runtime_printf();
  if (runtime_use_fopen)      runtime_fopen();
  if (runtime_use_fclose)     runtime_fclose();
  if (runtime_use_fgetc)      runtime_fgetc();
  if (runtime_use_read)       runtime_read();
  if (runtime_use_write)      runtime_write();
  if (runtime_use_open)       runtime_open();
  if (runtime_use_close)      runtime_close();
  if (runtime_use_make_argv)  runtime_make_argv();
  if (runtime_use_local_vars) runtime_local_vars();
  if (runtime_use_pack_string) runtime_pack_string();
  if (runtime_use_unpack_string) runtime_unpack_string();
}
