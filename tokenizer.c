#ifndef SIX_CC

#include <stdio.h>
#include <stdlib.h>
typedef FILE *FILE_ptr;
typedef int *int_ptr;
typedef char *char_ptr;

#endif


#define AVOID_AMPAMP_BARBAR
#define USE_IN_RANGE_FUNCTION_not
#define INLINE_get_ch

#ifdef AVOID_AMPAMP_BARBAR
#define AND &
#define OR |
#else
#define AND &&
#define OR ||
#endif

#ifdef USE_IN_RANGE_FUNCTION
int in_range(int x, int lo, int hi) {
  if (x < lo) return 0;
  if (x > hi) return 0;
  return 1;
}
#else
#define in_range(x, lo, hi) ((x >= lo) AND (x <= hi))
#endif


int NEWLINE = 10;
int SPACE = 32;
int SHARP = 35;
int DIGIT_0 = 48;
int DIGIT_9 = 57;
int UPPER_A = 65;
int UPPER_Z = 90;
int LOWER_A = 97;
int LOWER_Z = 122;
int UNDERSCORE = 95;

int INTEGER = 256;
int IDENTIFIER = 257;
int OTHER = 258;


int AUTO_KW     = 300;
int BREAK_KW    = 301;
int CASE_KW     = 302;
int CHAR_KW     = 303;
int CONST_KW    = 304;
int CONTINUE_KW = 305;
int DEFAULT_KW  = 306;
int DEFINE_KW   = 307;
int DO_KW       = 308;
int DOUBLE_KW   = 309;
int ELSE_KW     = 310;
int ENDIF_KW    = 311;
int ENUM_KW     = 312;
int ERROR_KW    = 313;
int EXTERN_KW   = 314;
int FLOAT_KW    = 315;
int FOR_KW      = 316;
int GOTO_KW     = 317;
int IF_KW       = 318;
int IFDEF_KW    = 319;
int IFNDEF_KW   = 320;
int INCLUDE_KW  = 321;
int INT_KW      = 322;
int LONG_KW     = 323;
int REGISTER_KW = 324;
int RETURN_KW   = 325;
int SHORT_KW    = 326;
int SIGNED_KW   = 327;
int SIZEOF_KW   = 328;
int STATIC_KW   = 329;
int STRUCT_KW   = 330;
int SWITCH_KW   = 331;
int TYPEDEF_KW  = 332;
int UNDEF_KW    = 333;
int UNION_KW    = 334;
int UNSIGNED_KW = 335;
int VOID_KW     = 336;
int VOLATILE_KW = 337;
int WHILE_KW    = 338;


void fatal_error(char_ptr msg) {
  printf("%s\n", msg);
  exit(1);
}

/* tokenizer */

void handle_preprocessor_directive() {
  return;
}

int ch;
int tok;
int val;

#define STRING_POOL_SIZE 10000
char string_pool[STRING_POOL_SIZE];
int string_pool_alloc = 0;

/* These parameters give a perfect hashing of the C keywords */
#define HASH_PARAM 12443
#define HASH_PRIME 103
#define IDENT_TABLE_SIZE 10000 /* MUST BE > HASH_PRIME */
int ident_table[IDENT_TABLE_SIZE];
int ident_table_alloc = HASH_PRIME;
int ident_start;
int hash;

int start_ident() {
  ident_start = string_pool_alloc;
  hash = 0;
}

void accum_ident() {
  string_pool[string_pool_alloc] = ch;
  string_pool_alloc += 1;
  hash = (ch + (hash ^ HASH_PARAM)) % HASH_PRIME;
  if (string_pool_alloc >= STRING_POOL_SIZE) {
    fatal_error("string pool overflow");
  }
}

int end_ident() {

  int probe;
  int probe_start;
  int c1;
  int c2;
  int i;

  string_pool[string_pool_alloc] = 0; /* terminate string */

  probe = ident_table[hash];

  while (probe != 0) {
    probe_start = ident_table[probe+1];
    i = 0;
    c1 = string_pool[ident_start+i];
    c2 = string_pool[probe_start+i];
    while (c1 == c2) {
      if (c1 == 0) {
        string_pool_alloc = ident_start; /* undo string allocation */
        return probe;
      }
      i += 1;
      c1 = string_pool[ident_start+i];
      c2 = string_pool[probe_start+i];
    }
    hash = probe; /* remember previous ident */
    probe = ident_table[probe];
  }

  /* a new ident has been found */

  probe = ident_table_alloc;
  ident_table_alloc += 3;

  if (ident_table_alloc > IDENT_TABLE_SIZE) {
    fatal_error("ident table overflow");
  }

  string_pool_alloc += 1; /* account for terminator */

  ident_table[hash] = probe; /* add new ident at end of chain */

  ident_table[probe] = 0; /* no next ident */
  ident_table[probe+1] = ident_start;
  ident_table[probe+2] = IDENTIFIER;

  return probe;
}

#ifdef INLINE_get_ch

#define get_ch() ch = getchar()

#else

void get_ch() {
  ch = getchar();
}

#endif

void get_ident() {

  start_ident();

  while (in_range(ch, UPPER_A, UPPER_Z) OR
         in_range(ch, LOWER_A, LOWER_Z) OR
         in_range(ch, DIGIT_0, DIGIT_9) OR
         (ch == UNDERSCORE)) {
    accum_ident();
    get_ch();
  }

  val = end_ident();
  tok = ident_table[val+2];
  /*
  printf("tok=%d val=%d %s\n", tok, val, string_pool + ident_table[val+1]);
  */
}

void init_kw(int tok, char_ptr name) {

  int i = 0;

  start_ident();

  while (name[i] != 0) {
    ch = name[i];
    i += 1;
    accum_ident();
  }

  ident_table[end_ident()+2] = tok;
}

void init_ident_table() {

  int i;

  for (i=0; i<HASH_PRIME; i++) {
    ident_table[i] = 0;
  }

  init_kw(AUTO_KW,     "auto");
  init_kw(BREAK_KW,    "break");
  init_kw(CASE_KW,     "case");
  init_kw(CHAR_KW,     "char");
  init_kw(CONST_KW,    "const");
  init_kw(CONTINUE_KW, "continue");
  init_kw(DEFAULT_KW,  "default");
  init_kw(DEFINE_KW,   "define");
  init_kw(DO_KW,       "do");
  init_kw(DOUBLE_KW,   "double");
  init_kw(ELSE_KW,     "else");
  init_kw(ENDIF_KW,    "endif");
  init_kw(ENUM_KW,     "enum");
  init_kw(ERROR_KW,    "error");
  init_kw(EXTERN_KW,   "extern");
  init_kw(FLOAT_KW,    "float");
  init_kw(FOR_KW,      "for");
  init_kw(GOTO_KW,     "goto");
  init_kw(IF_KW,       "if");
  init_kw(IFDEF_KW,    "ifdef");
  init_kw(IFNDEF_KW,   "ifndef");
  init_kw(INCLUDE_KW,  "include");
  init_kw(INT_KW,      "int");
  init_kw(LONG_KW,     "long");
  init_kw(REGISTER_KW, "register");
  init_kw(RETURN_KW,   "return");
  init_kw(SHORT_KW,    "short");
  init_kw(SIGNED_KW,   "signed");
  init_kw(SIZEOF_KW,   "sizeof");
  init_kw(STATIC_KW,   "static");
  init_kw(STRUCT_KW,   "struct");
  init_kw(SWITCH_KW,   "switch");
  init_kw(TYPEDEF_KW,  "typedef");
  init_kw(UNDEF_KW,    "undef");
  init_kw(UNION_KW,    "union");
  init_kw(UNSIGNED_KW, "unsigned");
  init_kw(VOID_KW,     "void");
  init_kw(VOLATILE_KW, "volatile");
  init_kw(WHILE_KW,    "while");
}

void get_tok() {

  while (1) {

    if (ch <= SPACE) {

      if (ch == EOF) {
        tok = EOF;
        break;
      }

      /* skip whitespace, detecting when it is at start of line */

      if (ch == NEWLINE) tok = ch;
      get_ch();

      while (in_range(ch, 0, SPACE)) {
        if (ch == NEWLINE) tok = ch;
        get_ch();
      }

      /* detect '#' at start of line, possibly preceded by whitespace */

      if ((tok == NEWLINE) AND (ch == SHARP))
        handle_preprocessor_directive();

    } else if (in_range(ch, LOWER_A, LOWER_Z) OR
               in_range(ch, UPPER_A, UPPER_Z) OR
               (ch == UNDERSCORE)) {

      get_ident();

      tok = IDENTIFIER;

      break;

    } else if (in_range(ch, DIGIT_0, DIGIT_9)) {

      val = ch - DIGIT_0;
      get_ch();

      while (in_range(ch, DIGIT_0, DIGIT_9)) {
        val = val * 10 + (ch - DIGIT_0);
        get_ch();
      }

      tok = INTEGER;

      break;

    } else {

      get_ch();

      tok = OTHER;

      break;

    }
  }
}

int main() {

  init_ident_table();

  ch = NEWLINE;
  get_tok();

  while (tok != EOF) {
    /*
    if (tok == IDENTIFIER)
      printf("IDENTIFIER\n");
    else if (tok == INTEGER)
      printf("INTEGER\n");
    else
      printf("OTHER\n");
    */
    get_tok();
  }

  return 0;
}
