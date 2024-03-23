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

int NEWLINE_CH     = 10;
int SPACE_CH       = 32;
int EXCL_CH        = 33;
int DQUOTE_CH      = 34;
int SHARP_CH       = 35;
int PERCENT_CH     = 37;
int AMP_CH         = 38;
int QUOTE_CH       = 39;
int LPAREN_CH      = 40;
int RPAREN_CH      = 41;
int STAR_CH        = 42;
int PLUS_CH        = 43;
int COMMA_CH       = 44;
int MINUS_CH       = 45;
int PERIOD_CH      = 46;
int SLASH_CH       = 47;
int DIGIT_0_CH     = 48;
int DIGIT_7_CH     = 55;
int DIGIT_9_CH     = 57;
int COLON_CH       = 58;
int SEMICOLON_CH   = 59;
int LT_CH          = 60;
int EQ_CH          = 61;
int GT_CH          = 62;
int QUESTION_CH    = 63;
int UPPER_A_CH     = 65;
int UPPER_F_CH     = 70;
int UPPER_X_CH     = 88;
int UPPER_Z_CH     = 90;
int CARET_CH       = 94;
int LBRACK_CH      = 91;
int BACKSLASH_CH   = 92;
int RBRACK_CH      = 93;
int UNDERSCORE_CH  = 95;
int LOWER_A_CH     = 97;
int LOWER_B_CH     = 98;
int LOWER_F_CH     = 102;
int LOWER_N_CH     = 110;
int LOWER_R_CH     = 114;
int LOWER_T_CH     = 116;
int LOWER_V_CH     = 118;
int LOWER_X_CH     = 120;
int LOWER_Z_CH     = 122;
int LBRACE_CH      = 123;
int BAR_CH         = 124;
int RBRACE_CH      = 125;
int TILDE_CH       = 126;

int AUTO_KW        = 300;
int BREAK_KW       = 301;
int CASE_KW        = 302;
int CHAR_KW        = 303;
int CONST_KW       = 304;
int CONTINUE_KW    = 305;
int DEFAULT_KW     = 306;
int DEFINE_KW      = 307;
int DO_KW          = 308;
int DOUBLE_KW      = 309;
int ELSE_KW        = 310;
int ENDIF_KW       = 311;
int ENUM_KW        = 312;
int ERROR_KW       = 313;
int EXTERN_KW      = 314;
int FLOAT_KW       = 315;
int FOR_KW         = 316;
int GOTO_KW        = 317;
int IF_KW          = 318;
int IFDEF_KW       = 319;
int IFNDEF_KW      = 320;
int INCLUDE_KW     = 321;
int INT_KW         = 322;
int LONG_KW        = 323;
int REGISTER_KW    = 324;
int RETURN_KW      = 325;
int SHORT_KW       = 326;
int SIGNED_KW      = 327;
int SIZEOF_KW      = 328;
int STATIC_KW      = 329;
int STRUCT_KW      = 330;
int SWITCH_KW      = 331;
int TYPEDEF_KW     = 332;
int UNDEF_KW       = 333;
int UNION_KW       = 334;
int UNSIGNED_KW    = 335;
int VOID_KW        = 336;
int VOLATILE_KW    = 337;
int WHILE_KW       = 338;

int IDENTIFIER_TOK = 400;
int INTEGER_TOK    = 401;
int CHARACTER_TOK  = 402;
int STRING_TOK     = 403;

int AMP_AMP_TOK    = 404;
int AMP_EQ_TOK     = 404;
int BAR_BAR_TOK    = 405;
int BAR_EQ_TOK     = 406;
int CARET_EQ_TOK   = 407;
int EQ_EQ_TOK      = 408;
int GT_EQ_TOK      = 409;
int LSHIFT_EQ_TOK  = 410;
int LSHIFT_TOK     = 411;
int LT_EQ_TOK      = 412;
int MINUS_EQ_TOK   = 413;
int EXCL_EQ_TOK    = 414;
int PERCENT_EQ_TOK = 415;
int PLUS_EQ_TOK    = 416;
int RSHIFT_EQ_TOK  = 417;
int RSHIFT_TOK     = 418;
int SLASH_EQ_TOK   = 419;
int STAR_EQ_TOK    = 420;


void fatal_error(char_ptr msg) {
  printf("%s\n", msg);
  exit(1);
}

/* tokenizer */

int ch;
int tok;
int val;

#define STRING_POOL_SIZE 100000
char string_pool[STRING_POOL_SIZE];
int string_pool_alloc = 0;
int string_start;
int hash;

/* These parameters give a perfect hashing of the C keywords */
#define HASH_PARAM 12443
#define HASH_PRIME 103
#define IDENT_TABLE_SIZE 100000 /* MUST BE > HASH_PRIME */
int ident_table[IDENT_TABLE_SIZE];
int ident_table_alloc = HASH_PRIME;

int begin_string() {
  string_start = string_pool_alloc;
  hash = 0;
}

void accum_string() {
  hash = (ch + (hash ^ HASH_PARAM)) % HASH_PRIME;
  string_pool[string_pool_alloc] = ch;
  string_pool_alloc += 1;
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
    c1 = string_pool[string_start+i];
    c2 = string_pool[probe_start+i];
    while (c1 == c2) {
      if (c1 == 0) {
        string_pool_alloc = string_start; /* undo string allocation */
        return probe;
      }
      i += 1;
      c1 = string_pool[string_start+i];
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
  ident_table[probe+1] = string_start;
  ident_table[probe+2] = IDENTIFIER_TOK;

  return probe;
}

#ifdef INLINE_get_ch

#define get_ch() ch = getchar()

#else

void get_ch() {
  ch = getchar();
}

#endif

void handle_preprocessor_directive() {
  /*TODO*/
  while ((ch != NEWLINE_CH) AND (ch != EOF)) {
    printf("%c", ch);
    get_ch();
  }
  printf(" \n");
}

void get_ident() {

  begin_string();

  while (in_range(ch, UPPER_A_CH, UPPER_Z_CH) OR
         in_range(ch, LOWER_A_CH, LOWER_Z_CH) OR
         in_range(ch, DIGIT_0_CH, DIGIT_9_CH) OR
         (ch == UNDERSCORE_CH)) {
    accum_string();
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

  begin_string();

  while (name[i] != 0) {
    ch = name[i];
    accum_string();
    i += 1;
  }

  ident_table[end_ident()+2] = tok;
}

void init_ident_table() {

  int i;

  for (i=0; i<HASH_PRIME; i += 1) {
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

int accum_digit(int base) {
  int digit = 99;
  int MININT = -2147483648;
  int limit;
  if (in_range(ch, DIGIT_0_CH, DIGIT_9_CH)) {
    digit = ch - DIGIT_0_CH;
  } else if (in_range(ch, UPPER_A_CH, UPPER_Z_CH)) {
    digit = ch - UPPER_A_CH + 10;
  } else if (in_range(ch, LOWER_A_CH, LOWER_Z_CH)) {
    digit = ch - LOWER_A_CH + 10;
  }
  if (digit >= base) {
    return 0; /* character is not a digit in that base */
  } else {
    limit = MININT / base;
    if ((val < limit) OR ((val == limit) AND (digit > limit * base - MININT))) {
      fatal_error("literal integer overflow");
    } else {
      val = val * base - digit;
      get_ch();
    }
    return 1;
  }
}

void get_string_char() {

  val = ch;
  get_ch();

  if (val == BACKSLASH_CH) {
    if (in_range(ch, DIGIT_0_CH, DIGIT_7_CH)) {
      /*
      Parse octal character, up to 3 digits.
      Note that \1111 is parsed as '\111' followed by '1'
      See https://en.wikipedia.org/wiki/Escape_sequences_in_C#Notes
      */
      val = 0;
      accum_digit(8);
      accum_digit(8);
      accum_digit(8);
      val = -(val % 256); /* keep low 8 bits, without overflowing */
    } else if ((ch == UPPER_X_CH) OR (ch == LOWER_X_CH)) {
      get_ch();
      val = 0;
      /* Allow 1 or 2 hex digits. */
      if (accum_digit(16)) {
        accum_digit(16);
      } else {
        fatal_error("invalid hex escape -- it must have at least one digit");
      }
      val = -(val % 256); /* keep low 8 bits, without overflowing */
    } else {
      if (ch == LOWER_A_CH) {
        val = 7;
      } else if (ch == LOWER_B_CH) {
        val = 8;
      } else if (ch == LOWER_F_CH) {
        val = 12;
      } else if (ch == LOWER_N_CH) {
        val = 10;
      } else if (ch == LOWER_R_CH) {
        val = 13;
      } else if (ch == LOWER_T_CH) {
        val = 9;
      } else if (ch == LOWER_V_CH) {
        val = 11;
      } else if ((ch == BACKSLASH_CH) OR (ch == QUOTE_CH) OR (ch == DQUOTE_CH)) {
        val = ch;
      } else {
        fatal_error("unimplemented string character escape");
      }
      get_ch();
    }
  }
}

void get_tok() {

  while (1) {

    if (ch <= SPACE_CH) {

      if (ch == EOF) {
        tok = EOF;
        break;
      }

      /* skip whitespace, detecting when it is at start of line */

      if (ch == NEWLINE_CH) tok = ch;
      get_ch();

      while (in_range(ch, 0, SPACE_CH)) {
        if (ch == NEWLINE_CH) tok = ch;
        get_ch();
      }

      /* detect '#' at start of line, possibly preceded by whitespace */

      if ((tok == NEWLINE_CH) AND (ch == SHARP_CH))
        handle_preprocessor_directive();

      /* will continue while (1) loop */

    } else if (in_range(ch, LOWER_A_CH, LOWER_Z_CH) OR
               in_range(ch, UPPER_A_CH, UPPER_Z_CH) OR
               (ch == UNDERSCORE_CH)) {

      get_ident();

      break;

    } else if (in_range(ch, DIGIT_0_CH, DIGIT_9_CH)) {

      val = DIGIT_0_CH - ch;

      get_ch();

      if (val == 0) { /* val == 0 <=> ch == DIGIT_0_CH */
        if ((ch == LOWER_X_CH) OR (ch == UPPER_X_CH)) {
          get_ch();
          val = 0;
          if (accum_digit(16)) {
            while (accum_digit(16)) ch = ch; /* dummy op */
          } else {
            fatal_error("invalid hex integer -- it must have at least one digit");
          }
        } else {
          while (accum_digit(8)) ch = ch; /* dummy op */
        }
      } else {
        while (accum_digit(10)) ch = ch; /* dummy op */
      }

      tok = INTEGER_TOK;

      break;

    } else if (ch == QUOTE_CH) {

      get_ch();
      get_string_char();

      if (ch != QUOTE_CH) {
        fatal_error("unterminated character literal");
      }

      get_ch();

      tok = CHARACTER_TOK;

      break;

    } else if (ch == DQUOTE_CH) {

      get_ch();

      begin_string();

      while ((ch != DQUOTE_CH) AND (ch != EOF)) {
        get_string_char();
        tok = ch;
        ch = val;
        accum_string();
        ch = tok;
      }

      if (ch != DQUOTE_CH) {
        fatal_error("unterminated string literal");
      }

      ch = 0;
      accum_string();

      get_ch();

      val = string_start;
      tok = STRING_TOK;

      break;

    } else {

      tok = ch; /* fallback for single char tokens */

      if (ch == SLASH_CH) {

        get_ch();
        if (ch == STAR_CH) {
          get_ch();
          tok = ch; /* remember previous char, except first one */
          while (((tok != STAR_CH) OR (ch != SLASH_CH)) AND (ch != EOF)) {
            tok = ch;
            get_ch();
          }
          if (ch == EOF) {
            fatal_error("unterminated comment");
          }
          get_ch();
          /* will continue while (1) loop */
        } else if (ch == SLASH_CH) {
          while ((ch != NEWLINE_CH) AND (ch != EOF)) {
            get_ch();
          }
          /* will continue while (1) loop */
        } else {
          if (ch == EQ_CH) {
            get_ch();
            tok = SLASH_EQ_TOK;
          }
          break;
        }

      } else if (ch == AMP_CH) {

        get_ch();
        if (ch == AMP_CH) {
          get_ch();
          tok = AMP_AMP_TOK;
        } else if (ch == EQ_CH) {
          get_ch();
          tok = AMP_EQ_TOK;
        }

        break;

      } else if (ch == BAR_CH) {

        get_ch();
        if (ch == BAR_CH) {
          get_ch();
          tok = BAR_BAR_TOK;
        } else if (ch == EQ_CH) {
          get_ch();
          tok = BAR_EQ_TOK;
        }

        break;

      } else if (ch == LT_CH) {

        get_ch();
        if (ch == EQ_CH) {
          get_ch();
          tok = LT_EQ_TOK;
        } else if (ch == LT_CH) {
          get_ch();
          if (ch == EQ_CH) {
            get_ch();
            tok = LSHIFT_EQ_TOK;
          } else {
            tok = LSHIFT_TOK;
          }
        }

        break;

      } else if (ch == GT_CH) {

        get_ch();
        if (ch == EQ_CH) {
          get_ch();
          tok = GT_EQ_TOK;
        } else if (ch == GT_CH) {
          get_ch();
          if (ch == EQ_CH) {
            get_ch();
            tok = RSHIFT_EQ_TOK;
          } else {
            tok = RSHIFT_TOK;
          }
        }

        break;

      } else if (ch == EQ_CH) {

        get_ch();
        if (ch == EQ_CH) {
          get_ch();
          tok = EQ_EQ_TOK;
        }

        break;

      } else if (ch == EXCL_CH) {

        get_ch();
        if (ch == EQ_CH) {
          get_ch();
          tok = EXCL_EQ_TOK;
        }

        break;

      } else if (ch == PLUS_CH) {

        get_ch();
        if (ch == EQ_CH) {
          get_ch();
          tok = PLUS_EQ_TOK;
        }

        break;

      } else if (ch == MINUS_CH) {

        get_ch();
        if (ch == EQ_CH) {
          get_ch();
          tok = MINUS_EQ_TOK;
        }

        break;

      } else if (ch == STAR_CH) {

        get_ch();
        if (ch == EQ_CH) {
          get_ch();
          tok = STAR_EQ_TOK;
        }

        break;

      } else if (ch == PERCENT_CH) {

        get_ch();
        if (ch == EQ_CH) {
          get_ch();
          tok = PERCENT_EQ_TOK;
        }

        break;

      } else if (ch == CARET_CH) {

        get_ch();
        if (ch == EQ_CH) {
          get_ch();
          tok = CARET_EQ_TOK;
        }

        break;

      } else if ((ch == QUESTION_CH) OR (ch == COMMA_CH) OR (ch == COLON_CH) OR (ch == SEMICOLON_CH) OR (ch == LPAREN_CH) OR (ch == RPAREN_CH) OR (ch == LBRACK_CH) OR (ch == RBRACK_CH) OR (ch == LBRACE_CH) OR (ch == RBRACE_CH)) {

        tok = ch;

        get_ch();

        break;

      } else {
        printf("ch=%c\n", ch);
        fatal_error("invalid token");
      }
    }
  }
}

void print_string_char(int c) {
  if (c == 7) printf("\\a");
  else if (c == 8) printf("\\b");
  else if (c == 12) printf("\\f");
  else if (c == 10) printf("\\n");
  else if (c == 13) printf("\\r");
  else if (c == 9) printf("\\t");
  else if (c == 11) printf("\\v");
  else if ((c == BACKSLASH_CH) OR (c == QUOTE_CH) OR (c == DQUOTE_CH)) printf("\\%c", c);
  else if ((c < 32) OR (c > 126)) printf("\\%d%d%d", c>>6, (c>>3)&7, c&7);
  else putchar(c);
}

void print_tok() {

  int i;

  if (tok == AUTO_KW) printf("auto");
  else if (tok == BREAK_KW) printf("break");
  else if (tok == CASE_KW) printf("case");
  else if (tok == CHAR_KW) printf("char");
  else if (tok == CONST_KW) printf("const");
  else if (tok == CONTINUE_KW) printf("continue");
  else if (tok == DEFAULT_KW) printf("default");
  else if (tok == DEFINE_KW) printf("define");
  else if (tok == DO_KW) printf("do");
  else if (tok == DOUBLE_KW) printf("double");
  else if (tok == ELSE_KW) printf("else");
  else if (tok == ENDIF_KW) printf("endif");
  else if (tok == ENUM_KW) printf("enum");
  else if (tok == ERROR_KW) printf("error");
  else if (tok == EXTERN_KW) printf("extern");
  else if (tok == FLOAT_KW) printf("float");
  else if (tok == FOR_KW) printf("for");
  else if (tok == GOTO_KW) printf("goto");
  else if (tok == IF_KW) printf("if");
  else if (tok == IFDEF_KW) printf("ifdef");
  else if (tok == IFNDEF_KW) printf("ifndef");
  else if (tok == INCLUDE_KW) printf("include");
  else if (tok == INT_KW) printf("int");
  else if (tok == LONG_KW) printf("long");
  else if (tok == REGISTER_KW) printf("register");
  else if (tok == RETURN_KW) printf("return");
  else if (tok == SHORT_KW) printf("short");
  else if (tok == SIGNED_KW) printf("signed");
  else if (tok == SIZEOF_KW) printf("sizeof");
  else if (tok == STATIC_KW) printf("static");
  else if (tok == STRUCT_KW) printf("struct");
  else if (tok == SWITCH_KW) printf("switch");
  else if (tok == TYPEDEF_KW) printf("typedef");
  else if (tok == UNDEF_KW) printf("undef");
  else if (tok == UNION_KW) printf("union");
  else if (tok == UNSIGNED_KW) printf("unsigned");
  else if (tok == VOID_KW) printf("void");
  else if (tok == VOLATILE_KW) printf("volatile");
  else if (tok == WHILE_KW) printf("while");

  else if (tok == AMP_AMP_TOK) printf("&&");
  else if (tok == AMP_EQ_TOK) printf("&=");
  else if (tok == BAR_BAR_TOK) printf("||");
  else if (tok == BAR_EQ_TOK) printf("|=");
  else if (tok == CARET_EQ_TOK) printf("^=");
  else if (tok == EQ_EQ_TOK) printf("==");
  else if (tok == GT_EQ_TOK) printf(">=");
  else if (tok == LSHIFT_EQ_TOK) printf("<<=");
  else if (tok == LSHIFT_TOK) printf("<<");
  else if (tok == LT_EQ_TOK) printf("<=");
  else if (tok == MINUS_EQ_TOK) printf("-=");
  else if (tok == EXCL_EQ_TOK) printf("!=");
  else if (tok == PERCENT_EQ_TOK) printf("%=");
  else if (tok == PLUS_EQ_TOK) printf("+=");
  else if (tok == RSHIFT_EQ_TOK) printf(">>=");
  else if (tok == RSHIFT_TOK) printf(">>");
  else if (tok == SLASH_EQ_TOK) printf("/=");
  else if (tok == STAR_EQ_TOK) printf("*=");

  else if (tok == IDENTIFIER_TOK) {
    printf("/*IDENTIFIER*/ %s", string_pool + ident_table[val+1]);
  } else if (tok == INTEGER_TOK) {
    printf("/*INTEGER*/ %d", -val);
  } else if (tok == CHARACTER_TOK) {
    printf("/*CHARACTER %d*/ '", val);
    print_string_char(val);
    printf("' ");
  } else if (tok == STRING_TOK) {
    printf("/*STRING*/ \"");
    i = 0;
    while (string_pool[val+i] != 0) {
      print_string_char(string_pool[val+i]);
      i += 1;
    }
    printf("\" ");
  } else {
    printf("%c ", tok);
  }
  printf(" \n");
}

int main() {

  init_ident_table();

  ch = NEWLINE_CH;
  get_tok();

  while (tok != EOF) {

    print_tok();

    get_tok();
  }
  /*
  printf("string_pool_alloc=%d ident_table_alloc=%d\n", string_pool_alloc, ident_table_alloc);
  */
  return 0;
}
