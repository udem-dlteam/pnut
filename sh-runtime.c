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
#define call_char_to_int(prefix, char_var) putstr(prefix "__c=$(LC_CTYPE=C printf \"%d\" \"'" char_var "\")\n");
#else
#define call_char_to_int(prefix, char_var) putstr(prefix "char_to_int \"" char_var "\"\n");
#endif

#ifdef RT_COMPACT
#define call_int_to_char(prefix, int_var) putstr(prefix "__char=$(printf \"\\\\$(printf \"%o\" \"" int_var "\")\")\n");
#else
#define call_int_to_char(prefix, int_var) putstr(prefix "int_to_char \"" int_var "\"\n");
#endif

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

// Local variables

DEFINE_RUNTIME_FUN(local_vars)
  printf("# Local variables\n");
  printf("__SP=0\n");
#ifdef SH_INDIVIDUAL_LET
  printf("let() { : $((__SP += 1)) $((__$__SP=$1)); } \n");
#else
  printf("let() { while [ $# -gt 0 ]; do : $((__SP += 1)) $((__$__SP=$1)) ; shift; done }\n");
#endif
  printf("endlet() {\n");
  printf("  # Make sure we don't overwrite the return location if it is part of the local variables\n");
  printf("  __return_loc=$1; shift\n");
  printf("  while [ $# -gt 0 ]; do\n");
  printf("    if [ $1 != \"$__return_loc\" ]; then : $(($1=__$__SP)); fi\n");
  printf("    : $((__SP -= 1)); shift\n");
  printf("  done\n");
  printf("}\n");
END_RUNTIME_FUN(local_vars)

// char<->int conversion

DEFINE_RUNTIME_FUN(int_to_char)
#ifndef RT_COMPACT
  putstr("int_to_char() {\n");
  putstr("  case $1 in\n");
  putstr("    48|49|50|51|52|53|54|55|56|57) __char=$(($1 - 48)) ;;\n");
  putstr("    97)  __char=\"a\" ;;\n");
  putstr("    98)  __char=\"b\" ;;\n");
  putstr("    99)  __char=\"c\" ;;\n");
  putstr("    100) __char=\"d\" ;;\n");
  putstr("    101) __char=\"e\" ;;\n");
  putstr("    102) __char=\"f\" ;;\n");
  putstr("    103) __char=\"g\" ;;\n");
  putstr("    104) __char=\"h\" ;;\n");
  putstr("    105) __char=\"i\" ;;\n");
  putstr("    106) __char=\"j\" ;;\n");
  putstr("    107) __char=\"k\" ;;\n");
  putstr("    108) __char=\"l\" ;;\n");
  putstr("    109) __char=\"m\" ;;\n");
  putstr("    110) __char=\"n\" ;;\n");
  putstr("    111) __char=\"o\" ;;\n");
  putstr("    112) __char=\"p\" ;;\n");
  putstr("    113) __char=\"q\" ;;\n");
  putstr("    114) __char=\"r\" ;;\n");
  putstr("    115) __char=\"s\" ;;\n");
  putstr("    116) __char=\"t\" ;;\n");
  putstr("    117) __char=\"u\" ;;\n");
  putstr("    118) __char=\"v\" ;;\n");
  putstr("    119) __char=\"w\" ;;\n");
  putstr("    120) __char=\"x\" ;;\n");
  putstr("    121) __char=\"y\" ;;\n");
  putstr("    122) __char=\"z\" ;;\n");
  putstr("    65)  __char=\"A\" ;;\n");
  putstr("    66)  __char=\"B\" ;;\n");
  putstr("    67)  __char=\"C\" ;;\n");
  putstr("    68)  __char=\"D\" ;;\n");
  putstr("    69)  __char=\"E\" ;;\n");
  putstr("    70)  __char=\"F\" ;;\n");
  putstr("    71)  __char=\"G\" ;;\n");
  putstr("    72)  __char=\"H\" ;;\n");
  putstr("    73)  __char=\"I\" ;;\n");
  putstr("    74)  __char=\"J\" ;;\n");
  putstr("    75)  __char=\"K\" ;;\n");
  putstr("    76)  __char=\"L\" ;;\n");
  putstr("    77)  __char=\"M\" ;;\n");
  putstr("    78)  __char=\"N\" ;;\n");
  putstr("    79)  __char=\"O\" ;;\n");
  putstr("    80)  __char=\"P\" ;;\n");
  putstr("    81)  __char=\"Q\" ;;\n");
  putstr("    82)  __char=\"R\" ;;\n");
  putstr("    83)  __char=\"S\" ;;\n");
  putstr("    84)  __char=\"T\" ;;\n");
  putstr("    85)  __char=\"U\" ;;\n");
  putstr("    86)  __char=\"V\" ;;\n");
  putstr("    87)  __char=\"W\" ;;\n");
  putstr("    88)  __char=\"X\" ;;\n");
  putstr("    89)  __char=\"Y\" ;;\n");
  putstr("    90)  __char=\"Z\" ;;\n");
  putstr("    32)  __char=\" \" ;;\n");
  putstr("    33)  __char=\"!\" ;;\n");
  putstr("    34)  __char=\"\\\"\" ;;\n");
  putstr("    35)  __char=\"#\" ;;\n");
  putstr("    36)  __char=\"$\" ;;\n");
  putstr("    37)  __char=\"%\" ;;\n");
  putstr("    38)  __char=\"&\" ;;\n");
  putstr("    39)  __char=\"'\" ;;\n");
  putstr("    40)  __char=\"(\" ;;\n");
  putstr("    41)  __char=\")\" ;;\n");
  putstr("    42)  __char=\"*\" ;;\n");
  putstr("    43)  __char=\"+\" ;;\n");
  putstr("    44)  __char=\",\" ;;\n");
  putstr("    45)  __char=\"-\" ;;\n");
  putstr("    46)  __char=\".\" ;;\n");
  putstr("    47)  __char=\"/\" ;;\n");
  putstr("    58)  __char=\":\" ;;\n");
  putstr("    59)  __char=\";\" ;;\n");
  putstr("    60)  __char=\"<\" ;;\n");
  putstr("    61)  __char=\"=\" ;;\n");
  putstr("    62)  __char=\">\" ;;\n");
  putstr("    63)  __char=\"?\" ;;\n");
  putstr("    64)  __char=\"@\" ;;\n");
  putstr("    91)  __char=\"[\" ;;\n");
  putstr("    92)  __char=\"\\\\\" ;;\n");
  putstr("    93)  __char=\"]\" ;;\n");
  putstr("    94)  __char=\"^\" ;;\n");
  putstr("    95)  __char=\"_\" ;;\n");
  putstr("    96)  __char=\"\\`\" ;;\n");
  putstr("    123) __char=\"{\" ;;\n");
  putstr("    124) __char=\"|\" ;;\n");
  putstr("    125) __char=\"}\" ;;\n");
  putstr("    126) __char=\"~\" ;;\n");
  putstr("    10)  __char=\"\\n\" ;;\n");
  putstr("    *)\n");
  putstr("      echo \"Invalid character code: $1\" ; exit 1\n");
  putstr("      __char=$(printf \"\\\\$(printf \"%o\" \"$1\")\") ;;\n");
  putstr("  esac\n");
  putstr("}\n");
#endif
END_RUNTIME_FUN(int_to_char)

DEFINE_RUNTIME_FUN(char_to_int)
#ifndef RT_COMPACT
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
  putstr("      __c=$(LC_CTYPE=C printf \"%d\" \"'$1\")\n");
  putstr("  esac\n");
  putstr("}\n");
#endif
END_RUNTIME_FUN(char_to_int)

// memory allocation
DEFINE_RUNTIME_FUN(alloc)
  putstr("__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.\n\n");
  putstr("alloc() {\n");
#ifdef RT_FREE_UNSETS_VARS
  // When free isn't a no-op, we need to tag all objects with their size
  putstr("  : $((_$__ALLOC = $1)) # Save allocation size\n");
  putstr("  : $((__ALLOC += 1))\n");
#endif
  putstr("  __addr=$__ALLOC\n");
  putstr("  : $((__ALLOC += $1))\n");
  putstr("}\n");
END_RUNTIME_FUN(alloc)

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
DEPENDS_ON(alloc)
#ifdef RT_NO_INIT_GLOBALS
  printf("defarr() { alloc $2; : $(( $1 = __addr )); }\n\n");
#else
DEPENDS_ON(initialize_memory)
  printf("defarr() { alloc $2; : $(( $1 = __addr )) ; initialize_memory $(($1)) $2; }\n\n");
#endif
END_RUNTIME_FUN(defarr)

DEFINE_RUNTIME_FUN(malloc)
DEPENDS_ON(alloc)
  putstr("_malloc() { # $2 = malloc_size\n");
  putstr("  alloc $2\n");
  putstr("  : $(($1 = __addr))\n");
  putstr("}\n");
END_RUNTIME_FUN(malloc)

DEFINE_RUNTIME_FUN(free)
  putstr("_free() { # $1 = pointer to object to free\n");
  putstr("  : $(($1 = 0)); shift # Return 0\n");
#ifdef RT_FREE_UNSETS_VARS
  putstr("  __ptr=$1\n");
  putstr("  __size=$((_$((__ptr - 1)))) # Get size of allocation\n");
  putstr("  while [ $__size -gt 0 ]; do\n");
  putstr("    unset \"_$__ptr\"\n");
  putstr("    : $((__ptr += 1))\n");
  putstr("    : $((__size -= 1))\n");
  putstr("  done\n");
#endif
  putstr("}\n");
END_RUNTIME_FUN(free)

// string packing/unpacking
DEFINE_RUNTIME_FUN(unpack_string)
DEPENDS_ON(alloc)
DEPENDS_ON(char_to_int)
  putstr("# Push a Shell string to the VM heap. Returns a reference to the string in $__addr.\n");
  putstr("unpack_string() {\n");
  putstr("  __buf=\"$1\"\n");
  putstr("  alloc $(( ${#__buf} + 1 ))\n");
  putstr("  __ptr=$__addr\n");
  putstr("  while [ -n \"$__buf\" ] ; do\n");
  putstr("    __char=\"${__buf%\"${__buf#?}\"}\"   # remove all but first char\n");
  putstr("    __buf=\"${__buf#?}\"               # remove the current char from $__buf\n");
  call_char_to_int("    ", "$__char")
  putstr("    : $((_$__ptr = __c))\n");
  putstr("    : $((__ptr += 1))\n");
  putstr("  done\n");
  putstr("  : $((_$__ptr = 0 ))\n");
  putstr("}\n");
END_RUNTIME_FUN(unpack_string)

// argv
DEFINE_RUNTIME_FUN(make_argv)
DEPENDS_ON(unpack_string)
  putstr("make_argv() {\n");
  putstr("  __argc=$1; shift;\n");
  putstr("  alloc $__argc # Allocate enough space for all elements. No need to initialize.\n");
  putstr("  __argv=$__addr # Saving address because its overwritten by unpack_string\n");
  putstr("  __argv_ptr=$__addr # __ptr is used by unpack_string\n");
  putstr("\n");
  putstr("  while [ $# -ge 1 ]; do\n");
  putstr("    unpack_string \"$1\"\n");
  putstr("    : $((_$__argv_ptr = $__addr))\n");
  putstr("    : $((__argv_ptr += 1))\n");
  putstr("    shift\n");
  putstr("  done\n");
  putstr("}\n");
END_RUNTIME_FUN(make_argv)

// See https://en.wikipedia.org/wiki/Escape_sequences_in_C#Table_of_escape_sequences
DEFINE_RUNTIME_FUN(unpack_escaped_string)
DEPENDS_ON(alloc)
DEPENDS_ON(char_to_int)
  putstr("unpack_escaped_string() {\n");
  putstr("  __buf=\"$1\"\n");
  putstr("  # Allocates enough space for all characters, assuming that no character is escaped\n");
  putstr("  alloc $(( ${#__buf} + 1 ))\n");
  putstr("  __ptr=$__addr\n");
  putstr("  while [ -n \"$__buf\" ] ; do\n");
  putstr("    case \"$__buf\" in\n");
  putstr("      '\\'*)\n");
  putstr("        __buf=\"${__buf#?}\"               # remove the current char from $__buf\n");
  putstr("        case \"$__buf\" in\n");
  putstr("          'a'*) __c=7 ;;\n");
  putstr("          'b'*) __c=8 ;;\n");
  putstr("          'f'*) __c=12 ;;\n");
  putstr("          'n'*) __c=10 ;;\n");
  putstr("          'r'*) __c=13 ;;\n");
  putstr("          't'*) __c=9 ;;\n");
  putstr("          'v'*) __c=11 ;;\n");
  putstr("          '\\'*) __c=92 ;;\n");
  putstr("          '\"'*) __c=34 ;;\n");
  putstr("          \"'\"*) __c=39 ;;\n");
  putstr("          '?'*) __c=63 ;;\n");
  putstr("          '$'*) __c=36 ;; # Not in C, used to escape variable expansion between double quotes\n");
  putstr("          *) echo \"invalid escape in string: $__char\"; exit 1 ;;\n");
  putstr("        esac\n");
  putstr("        __buf=\"${__buf#?}\"               # remove the current char from $__buf\n");
  putstr("        ;;\n");
  putstr("      *)\n");
  call_char_to_int("        ", "${__buf%\"${__buf#?}\"}")
  putstr("        __buf=\"${__buf#?}\"                  # remove the current char from $__buf\n");
  putstr("        ;;\n");
  putstr("    esac\n");
  putstr("    : $((_$__ptr = __c))\n");
  putstr("    : $((__ptr += 1))\n");
  putstr("  done\n");
  putstr("  : $((_$__ptr = 0 ))\n");
  putstr("}\n");
END_RUNTIME_FUN(unpack_escaped_string)

DEFINE_RUNTIME_FUN(pack_string)
DEPENDS_ON(int_to_char)
  putstr("# Convert a VM string reference to a Shell string.\n");
  putstr("# $__res is set to the result, and $__len is set to the length of the string.\n");
  putstr("pack_string() {\n");
  putstr("  __addr=$1; shift\n");
  putstr("  __max_len=100000000\n");
  putstr("  __delim=0\n");
  putstr("  __len=0\n");
  putstr("  __res=\"\"\n");
  putstr("  if [ $# -ge 1 ] ; then __delim=$1   ; shift ; fi # Optional end of string delimiter\n");
  putstr("  if [ $# -ge 1 ] ; then __max_len=$1 ; shift ; fi # Optional max length\n");
  putstr("  while [ $((_$__addr)) -ne $__delim ] && [ $__max_len -gt $__len ] ; do\n");
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
  putstr("    : $(( $1 = __addr ))\n");
  putstr("  fi\n");
  putstr("  set -u\n");
  putstr("}\n");
END_RUNTIME_FUN(defstr)

DEFINE_RUNTIME_FUN(exit)
  putstr("_exit() {\n");
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
  putstr("__stdin_line_ends_with_oef=0\n");
  putstr("_getchar() {\n");
  putstr("  if [ -z \"$__stdin_buf\" ] ; then                   # need to get next line when buffer empty\n");
  putstr("    if [ $__stdin_line_ends_with_oef -eq 1 ]; then  # EOF at end of line, return -1\n");
  putstr("      : $(($1 = -1))\n");
  putstr("      __stdin_line_ends_with_oef=0                  # Reset EOF flag for next getchar call\n");
  putstr("      return\n");
  putstr("    fi\n");
  putstr("    IFS=                                            # don't split input\n");
  putstr("    if read -r __stdin_buf ; then                   # read next line into $__stdin_buf\n");
  putstr("      if [ -z \"$__stdin_buf\" ] ; then               # an empty line implies a newline character\n");
  putstr("        : $(($1 = 10))                              # next getchar call will read next line\n");
  putstr("        return\n");
  putstr("      fi\n");
  putstr("    else\n");
  putstr("      if [ -z \"$__stdin_buf\" ] ; then               # EOF reached when read fails\n");
  putstr("        : $(($1 = -1))\n");
  putstr("        return\n");
  putstr("      else\n");
  putstr("        __stdin_line_ends_with_oef=1\n");
  putstr("      fi\n");
  putstr("    fi\n");
  putstr("  else\n");
  putstr("    __stdin_buf=\"${__stdin_buf#?}\"                  # remove the current char from $__stdin_buf\n");
  putstr("    if [ -z \"$__stdin_buf\" ] ; then                 # end of line if the buffer is now empty\n");
  putstr("      : $(($1 = 10))\n");
  putstr("      return\n");
  putstr("    fi\n");
  putstr("  fi\n");
  putstr("\n");
  extract_first_char("", "__stdin_buf", "$1")
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
  putstr("  while [ $(( _$__addr )) -ne 0 ]; do\n");
#ifdef RT_INLINE_PUTCHAR
  putstr("    printf \\\\$((_$__addr/64))$((_$__addr/8%8))$((_$__addr%8))\n");
#else
  putstr("    _putchar __ $((_$__addr))\n");
#endif
  putstr("    : $(( __addr += 1 ))\n");
  putstr("  done\n");
  putstr("}\n");
END_RUNTIME_FUN(print_pnut_str)

DEFINE_RUNTIME_FUN(print_string)
DEPENDS_ON(int_to_char)
  putstr("# Emit a C-string line by line so that whitespace isn't mangled\n");
  putstr("print_string() {\n");
  putstr("  __addr=$1; shift\n");
  putstr("  __max_len=100000000\n");
  putstr("  __delim=0\n");
  putstr("  __len=0\n");
  putstr("  __acc=\"\"\n");
  putstr("  if [ $# -ge 1 ] ; then __delim=$1   ; shift ; fi # Optional end of string delimiter\n");
  putstr("  if [ $# -ge 1 ] ; then __max_len=$1 ; shift ; fi # Optional max length\n");
  putstr("  while [ $((_$__addr)) -ne $__delim ] && [ $__max_len -gt $__len ] ; do\n");
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
DEPENDS_ON(int_to_char)
  putstr("_printf() { # $1 = printf format string, $2... = printf args\n");
  putstr("  : $(($1 = 0)); shift # Return 0\n");
  putstr("  __fmt_ptr=$1; shift\n");
  putstr("  __mod=0\n");
  putstr("  while [ \"$((_$__fmt_ptr))\" -ne 0 ] ; do\n");
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

// exec $fd<&- does not work as expected, and we don't want to use eval so we
// instead have a case statement that calls the appropriate exec command to open
// and close file descriptors.
DEFINE_RUNTIME_FUN(fopen)
DEPENDS_ON(alloc)
DEPENDS_ON(pack_string)
  putstr("__fopen_fd3=0\n");
  putstr("__fopen_fd4=0\n");
  putstr("__fopen_fd5=0\n");
  putstr("__fopen_fd6=0\n");
  putstr("__fopen_fd7=0\n");
  putstr("__fopen_fd8=0\n");
  putstr("__fopen_fd9=0\n");
  putstr("\n");
  putstr("next_fd() {\n");
  putstr("  __i=3\n");
  putstr("  while [ $__i -lt 10 ]; do\n");
  putstr("    if [ $((__fopen_fd$__i)) -eq 0 ]; then\n");
  putstr("      __fd=$__i\n");
  putstr("      return\n");
  putstr("    fi\n");
  putstr("    : $((__i += 1))\n");
  putstr("  done\n");
  putstr("  # Some shells don't support fd > 9\n");
  putstr("  echo \"No more file descriptors available\" ; exit 1\n");
  putstr("}\n");
  putstr("\n");
  putstr("open_fd() { # $1: fd id, $2: file to open\n");
  putstr("  : $((__fopen_fd$1 = 1)) # Mark the fd as opened\n");
  putstr("  case $1 in\n");
  putstr("    1) exec 1< $2 ;;\n");
  putstr("    2) exec 2< $2 ;;\n");
  putstr("    3) exec 3< $2 ;;\n");
  putstr("    4) exec 4< $2 ;;\n");
  putstr("    5) exec 5< $2 ;;\n");
  putstr("    6) exec 6< $2 ;;\n");
  putstr("    7) exec 7< $2 ;;\n");
  putstr("    8) exec 8< $2 ;;\n");
  putstr("    9) exec 9< $2 ;;\n");
  putstr("    *) echo \"Unknown fd: $1\"; exit 1 ;;\n");
  putstr("  esac\n");
  putstr("}\n");
  putstr("\n");
  putstr("# Read the file, and return a file descriptor to the file.\n");
  putstr("# The file descriptor fields:\n");
  putstr("# - 0: Buffer\n");
  putstr("# - 1: Read cursor\n");
  putstr("# - 2: Buffer size\n");
  putstr("# - 3: File descriptor number\n");
  putstr("# - 4: EOF?\n");
  putstr("# Because the file must be read line-by-line, and string values can't be\n");
  putstr("# assigned to dynamic variables, each line is read and then unpacked in the\n");
  putstr("# buffer.\n");
  putstr("_fopen() { # $2: File name, $3: Mode\n");
  putstr("  pack_string $2\n");
  putstr("  next_fd                       # Get available fd\n");
  putstr("  open_fd $__fd $__res\n");
  putstr("  alloc 4                       # Allocate file descriptor object\n");
  putstr("  : $(( $1 = __addr ))\n");
  putstr("  alloc 1000                    # Allocate buffer\n");
  putstr("  : $(( _$((__addr)) = 0 ))     # Initialize buf to \"\"\n");
  putstr("  : $((_$(($1 + 0)) = __addr))  # Save buffer address\n");
  putstr("  : $((_$(($1 + 1)) = 0))       # Initialize cursor to 0\n");
  putstr("  : $((_$(($1 + 2)) = 200))     # Initial buffer size is 1000\n");
  putstr("  : $((_$(($1 + 3)) = __fd))    # Save fd id\n");
  putstr("}\n");
END_RUNTIME_FUN(fopen)

DEFINE_RUNTIME_FUN(fclose)
DEPENDS_ON(fopen)
DEPENDS_ON(free)
  putstr("close_fd() { # $1: fd id\n");
  putstr(": $((__fopen_fd$1 = 0)) # Mark the fd as closed\n");
  putstr("  case $1 in\n");
  putstr("    1) exec 1<&- ;;\n");
  putstr("    2) exec 2<&- ;;\n");
  putstr("    3) exec 3<&- ;;\n");
  putstr("    4) exec 4<&- ;;\n");
  putstr("    5) exec 5<&- ;;\n");
  putstr("    6) exec 6<&- ;;\n");
  putstr("    7) exec 7<&- ;;\n");
  putstr("    8) exec 8<&- ;;\n");
  putstr("    9) exec 9<&- ;;\n");
  putstr("    *) echo \"Unknown fd: $1\"; exit 1 ;;\n");
  putstr("  esac\n");
  putstr("}\n");
  putstr("\n");
  putstr("_fclose() { # $2: File descriptor\n");
  putstr("  __fd_id=$((_$(($2 + 3)) ))    # Fd id is at offset 3\n");
  putstr("  __buf=$((_$(($2 + 0)) ))      # Buffer starts at offset 1\n");
  putstr("  _free __ $__buf               # Release file descriptor buffer\n");
  putstr("  _free __ $2                   # Release file descriptor object\n");
  putstr("  close_fd $__fd_id\n");
  putstr("  : $(($1 = 0))\n");
  putstr("}\n");
END_RUNTIME_FUN(fclose)

DEFINE_RUNTIME_FUN(fgetc)
DEPENDS_ON(char_to_int)
  putstr("# Unpack a Shell string into an appropriately sized buffer\n");
  putstr("unpack_line() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?\n");
  putstr("  __fgetc_buf=$1\n");
  putstr("  __buf=$2\n");
  putstr("  __ends_with_eof=$3\n");
  putstr("  while [ ! -z \"$__fgetc_buf\" ]; do\n");
  extract_first_char("  ", "__fgetc_buf", "_$__buf")
  putstr("    __fgetc_buf=${__fgetc_buf#?}      # Remove the first character\n");
  putstr("    : $((__buf += 1))                 # Move to the next buffer position\n");
  putstr("  done\n");
  putstr("\n");
  putstr("  if [ $__ends_with_eof -eq 0 ]; then # Ends with newline and not EOF?\n");
  putstr("    : $(( _$__buf = 10))              # Line end with newline\n");
  putstr("    : $((__buf += 1))\n");
  putstr("  fi\n");
  putstr("  : $(( _$__buf = 0))                 # Then \\0\n");
  putstr("}\n");
  putstr("\n");
  putstr("refill_buffer() { # $1: File descriptor\n");
  putstr("  __fd=$1\n");
  putstr("  __buf=$((_$((__fd + 0))))\n");
  putstr("  __fd_id=$((_$((__fd + 3))))\n");
  putstr("\n");
  putstr("  IFS=\n");
  putstr("  if read -r __fgetc_buf <&$__fd_id ; then  # read next line into $__fgetc_buf\n");
  putstr("    __ends_with_eof=0\n");
  putstr("  else\n");
  putstr("    __ends_with_eof=1\n");
  putstr("  fi\n");
  putstr("\n");
  putstr("  # Check that the buffer is large enough to unpack the line\n");
  putstr("  __buf_size=$((_$((__fd + 2)) - 2)) # Minus 2 to account for newline and \\0\n");
  putstr("  __len=${#__fgetc_buf}\n");
  putstr("  if [ $__len -gt $__buf_size ]; then\n");
  putstr("    # Free buffer and reallocate a new one double the line size\n");
  putstr("    __buf_size=$((__len * 2))\n");
  putstr("    _free __ $__buf\n");
  putstr("    alloc $__buf_size\n");
  putstr("    : $((_$((__fd + 0)) = __addr))\n");
  putstr("    : $((_$((__fd + 2)) = __buf_size))\n");
  putstr("    __buf=$__addr\n");
  putstr("  fi\n");
  putstr("  unpack_line \"$__fgetc_buf\" $__buf $__ends_with_eof\n");
  putstr("}\n");
  putstr("\n");
  putstr("_fgetc() { # $2: File descriptor\n");
  putstr("  __fd=$2\n");
  putstr("  __buf=$((_$((__fd + 0))))\n");
  putstr("  __cur=$((_$((__fd + 1))))\n");
  putstr("  # The cursor is at the end of the buffer, we need to read the next line\n");
  putstr("  if [ $((_$((__buf + __cur)))) -eq 0 ]; then\n");
  putstr("    # Buffer has been read completely, read next line\n");
  putstr("    refill_buffer $__fd\n");
  putstr("    __cur=0 # Reset cursor and reload fd fields\n");
  putstr("    __buf=$((_$((__fd + 0)))) # Reload buffer in case it was reallocated\n");
  putstr("    if [ $((_$((__buf + __cur)))) -eq 0 ]; then\n");
  putstr("      : $(($1 = -1)) # EOF\n");
  putstr("      return\n");
  putstr("    fi\n");
  putstr("  fi\n");
  putstr("  : $(($1 = _$((__buf + __cur))))\n");
  putstr("  : $((_$((__fd + 1)) = __cur + 1))      # Increment cursor\n");
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
  if (runtime_use_make_argv)  runtime_make_argv();
  if (runtime_use_local_vars) runtime_local_vars();
}
