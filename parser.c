#ifndef SIX_CC

#include <stdio.h>
#include <stdlib.h>
typedef FILE *FILE_ptr;
typedef int *int_ptr;
typedef char *char_ptr;
typedef void *void_ptr;

#endif

#define ast int


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

int IDENTIFIER = 400;
int INTEGER    = 401;
int CHARACTER  = 402;
int STRING     = 403;

int AMP_AMP    = 404;
int AMP_EQ     = 404;
int ARROW      = 405;
int BAR_BAR    = 406;
int BAR_EQ     = 407;
int CARET_EQ   = 408;
int EQ_EQ      = 409;
int GT_EQ      = 410;
int LSHIFT_EQ  = 411;
int LSHIFT     = 412;
int LT_EQ      = 413;
int MINUS_EQ   = 414;
int MINUS_MINUS= 415;
int EXCL_EQ    = 416;
int PERCENT_EQ = 417;
int PLUS_EQ    = 418;
int PLUS_PLUS  = 419;
int RSHIFT_EQ  = 420;
int RSHIFT     = 421;
int SLASH_EQ   = 422;
int STAR_EQ    = 423;

int STRING_TREE = 424;
int STRING_TREE_INTEGER = 425;
int STRING_TREE_CHAR = 426;
int STRING_TREE_STRING = 427;
int STRING_TREE_STRING_POOL = 428;

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
#define HEAP_SIZE 100000 /* MUST BE > HASH_PRIME */
int heap[HEAP_SIZE];
int heap_alloc = HASH_PRIME;

int alloc_result;

int alloc_obj(int size) {

  alloc_result = heap_alloc;

  heap_alloc += size;

  if (heap_alloc > HEAP_SIZE) {
    fatal_error("heap overflow");
  }

  return alloc_result;
}

void begin_string() {
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

int probe;
int probe_start;
int c1;
int c2;
int end_ident_i;

int end_ident() {
  string_pool[string_pool_alloc] = 0; /* terminate string */
  string_pool_alloc += 1; /* account for terminator */

  probe = heap[hash];

  while (probe != 0) {
    probe_start = heap[probe+1];
    end_ident_i = 0;
    c1 = string_pool[string_start+end_ident_i];
    c2 = string_pool[probe_start+end_ident_i];
    while (c1 == c2) {
      if (c1 == 0) {
        string_pool_alloc = string_start; /* undo string allocation */
        return probe;
      }
      end_ident_i += 1;
      c1 = string_pool[string_start+end_ident_i];
      c2 = string_pool[probe_start+end_ident_i];
    }
    hash = probe; /* remember previous ident */
    probe = heap[probe];
  }

  /* a new ident has been found */

  probe = alloc_obj(3);

  heap[hash] = probe; /* add new ident at end of chain */

  heap[probe] = 0; /* no next ident */
  heap[probe+1] = string_start;
  heap[probe+2] = IDENTIFIER;

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
  while ((ch != '\n') AND (ch != EOF)) {
    printf("%c", ch);
    get_ch();
  }
  printf(" \n");
}

void get_ident() {

  begin_string();

  while (in_range(ch, 'A', 'Z') OR
         in_range(ch, 'a', 'z') OR
         in_range(ch, '0', '9') OR
         (ch == '_')) {
    accum_string();
    get_ch();
  }

  val = end_ident();
  tok = heap[val+2];
  /*
  printf("tok=%d val=%d %s\n", tok, val, string_pool + heap[val+1]);
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

  heap[end_ident()+2] = tok;
}

void init_ident_table() {

  int i;

  for (i=0; i<HASH_PRIME; i += 1) {
    heap[i] = 0;
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
  if (in_range(ch, '0', '9')) {
    digit = ch - '0';
  } else if (in_range(ch, 'A', 'Z')) {
    digit = ch - 'A' + 10;
  } else if (in_range(ch, 'a', 'z')) {
    digit = ch - 'a' + 10;
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

  if (val == '\\') {
    if (in_range(ch, '0', '7')) {
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
    } else if ((ch == 'x') OR (ch == 'X')) {
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
      if (ch == 'a') {
        val = 7;
      } else if (ch == 'b') {
        val = 8;
      } else if (ch == 'f') {
        val = 12;
      } else if (ch == 'n') {
        val = 10;
      } else if (ch == 'r') {
        val = 13;
      } else if (ch == 't') {
        val = 9;
      } else if (ch == 'v') {
        val = 11;
      } else if ((ch == '\\') OR (ch == '\'') OR (ch == '\"')) {
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

    if (ch <= ' ') {

      if (ch == EOF) {
        tok = EOF;
        break;
      }

      /* skip whitespace, detecting when it is at start of line */

      if (ch == '\n') tok = ch;
      get_ch();

      while (in_range(ch, 0, ' ')) {
        if (ch == '\n') tok = ch;
        get_ch();
      }

      /* detect '#' at start of line, possibly preceded by whitespace */

      if ((tok == '\n') AND (ch == '#'))
        handle_preprocessor_directive();

      /* will continue while (1) loop */

    } else if (in_range(ch, 'a', 'z') OR
               in_range(ch, 'A', 'Z') OR
               (ch == '_')) {

      get_ident();

      break;

    } else if (in_range(ch, '0', '9')) {

      val = '0' - ch;

      get_ch();

      if (val == 0) { /* val == 0 <=> ch == '0' */
        if ((ch == 'x') OR (ch == 'X')) {
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

      tok = INTEGER;

      break;

    } else if (ch == '\'') {

      get_ch();
      get_string_char();

      if (ch != '\'') {
        fatal_error("unterminated character literal");
      }

      get_ch();

      tok = CHARACTER;

      break;

    } else if (ch == '\"') {

      get_ch();

      begin_string();

      while ((ch != '\"') AND (ch != EOF)) {
        get_string_char();
        tok = ch;
        ch = val;
        accum_string();
        ch = tok;
      }

      if (ch != '\"') {
        fatal_error("unterminated string literal");
      }

      ch = 0;
      accum_string();

      get_ch();

      val = string_start;
      tok = STRING;

      break;

    } else {

      tok = ch; /* fallback for single char tokens */

      if (ch == '/') {

        get_ch();
        if (ch == '*') {
          get_ch();
          tok = ch; /* remember previous char, except first one */
          while (((tok != '*') OR (ch != '/')) AND (ch != EOF)) {
            tok = ch;
            get_ch();
          }
          if (ch == EOF) {
            fatal_error("unterminated comment");
          }
          get_ch();
          /* will continue while (1) loop */
        } else if (ch == '/') {
          while ((ch != '\n') AND (ch != EOF)) {
            get_ch();
          }
          /* will continue while (1) loop */
        } else {
          if (ch == '=') {
            get_ch();
            tok = SLASH_EQ;
          }
          break;
        }

      } else if (ch == '&') {

        get_ch();
        if (ch == '&') {
          get_ch();
          tok = AMP_AMP;
        } else if (ch == '=') {
          get_ch();
          tok = AMP_EQ;
        }

        break;

      } else if (ch == '|') {

        get_ch();
        if (ch == '|') {
          get_ch();
          tok = BAR_BAR;
        } else if (ch == '=') {
          get_ch();
          tok = BAR_EQ;
        }

        break;

      } else if (ch == '<') {

        get_ch();
        if (ch == '=') {
          get_ch();
          tok = LT_EQ;
        } else if (ch == '<') {
          get_ch();
          if (ch == '=') {
            get_ch();
            tok = LSHIFT_EQ;
          } else {
            tok = LSHIFT;
          }
        }

        break;

      } else if (ch == '>') {

        get_ch();
        if (ch == '=') {
          get_ch();
          tok = GT_EQ;
        } else if (ch == '>') {
          get_ch();
          if (ch == '=') {
            get_ch();
            tok = RSHIFT_EQ;
          } else {
            tok = RSHIFT;
          }
        }

        break;

      } else if (ch == '=') {

        get_ch();
        if (ch == '=') {
          get_ch();
          tok = EQ_EQ;
        }

        break;

      } else if (ch == '!') {

        get_ch();
        if (ch == '=') {
          get_ch();
          tok = EXCL_EQ;
        }

        break;

      } else if (ch == '+') {

        get_ch();
        if (ch == '=') {
          get_ch();
          tok = PLUS_EQ;
        } else if (ch == '+') {
          get_ch();
          tok = PLUS_PLUS;
        }

        break;

      } else if (ch == '-') {

        get_ch();
        if (ch == '=') {
          get_ch();
          tok = MINUS_EQ;
        } else if (ch == '>') {
          get_ch();
          tok = ARROW;
        } else if (ch == '-') {
          get_ch();
          tok = MINUS_MINUS;
        }

        break;

      } else if (ch == '*') {

        get_ch();
        if (ch == '=') {
          get_ch();
          tok = STAR_EQ;
        }

        break;

      } else if (ch == '%') {

        get_ch();
        if (ch == '=') {
          get_ch();
          tok = PERCENT_EQ;
        }

        break;

      } else if (ch == '^') {

        get_ch();
        if (ch == '=') {
          get_ch();
          tok = CARET_EQ;
        }

        break;

      } else if ((ch == '~') OR (ch == '.') OR (ch == '?') OR (ch == ',') OR (ch == ':') OR (ch == ';') OR (ch == '(') OR (ch == ')') OR (ch == '[') OR (ch == ']') OR (ch == '{') OR (ch == '}')) {

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

/* parser */

int get_op(ast node) {
  return heap[node] & 1023;
}

ast get_nb_children(ast node) {
  return heap[node] >> 10;
}

int get_val(ast node) {
  return heap[node+1];
}

void set_val(ast node, int val) {
  heap[node+1] = val;
}

ast get_child(ast node, int i) {
  return heap[node+i+1];
}

void set_child(ast node, int i, ast child) {
  heap[node+i+1] = child;
}

ast ast_result;

ast new_ast0(int op, int val) {

  ast_result = alloc_obj(2);

  heap[ast_result] = op;
  set_val(ast_result, val);

  return ast_result;
}

ast new_ast1(int op, ast child0) {

  ast_result = alloc_obj(2);

  heap[ast_result] = op + 1024;
  set_child(ast_result, 0, child0);

  return ast_result;
}

ast new_ast2(int op, ast child0, ast child1) {

  ast_result = alloc_obj(3);

  heap[ast_result] = op + 2048;
  set_child(ast_result, 0, child0);
  set_child(ast_result, 1, child1);

  return ast_result;
}

ast new_ast3(int op, ast child0, ast child1, ast child2) {

  ast_result = alloc_obj(3);

  heap[ast_result] = op + 3072;
  set_child(ast_result, 0, child0);
  set_child(ast_result, 1, child1);
  set_child(ast_result, 2, child2);

  return ast_result;
}

ast new_ast4(int op, ast child0, ast child1, ast child2, ast child3) {

  ast_result = alloc_obj(4);

  heap[ast_result] = op + 4096;
  set_child(ast_result, 0, child0);
  set_child(ast_result, 1, child1);
  set_child(ast_result, 2, child2);
  set_child(ast_result, 3, child3);

  return ast_result;
}

void syntax_error(char_ptr msg) {
  printf("syntax error: %s\n", msg);
  fatal_error("syntax error");
}

void missing_feature_error(char_ptr msg) {
  printf("not yet implemented: %s\n", msg);
  fatal_error("syntax error");
}

void expect_tok(int expected_tok) {
  if (tok != expected_tok) {
    syntax_error("unexpected token");
  }
  get_tok();
}

#ifndef SIX_CC
ast parse_comma_expression();
#endif

ast parse_parenthesized_expression() {

  ast result;

  expect_tok('(');

  result = parse_comma_expression();

  expect_tok(')');

  return result;
}

ast parse_primary_expression() {

  ast result;

  if (tok == IDENTIFIER) {

    result = new_ast0(IDENTIFIER, val);
    get_tok();

  } else if (tok == INTEGER) {

    result = new_ast0(INTEGER, val);
    get_tok();

  } else if (tok == CHARACTER) {

    result = new_ast0(CHARACTER, val);
    get_tok();

  } else if (tok == STRING) {

    result = new_ast0(STRING, val);
    get_tok();
    /*TODO: contiguous strings*/

  } else if (tok == '(') {

    result = parse_parenthesized_expression();

  } else {
    syntax_error("identifier, literal, or '(' expected");
  }

  return result;
}

ast parse_postfix_expression() {

  ast result;
  ast child;

  result = parse_primary_expression();

  while (1) {
    if (tok == '[') {

      get_tok();
      child = parse_comma_expression();
      result = new_ast2('[', result, child);
      expect_tok(']');

    } else if (tok == '(') {

      get_tok();
      if (tok == ')') {
        child = 0;
      } else {
        child = parse_comma_expression();
      }
      result = new_ast2('(', result, child);
      expect_tok(')');

    } else if (tok == '.') {

      syntax_error("Struct/Union not supported");

    } else if (tok == ARROW) {

      syntax_error("Struct/Union not supported");

    } else if ((tok == PLUS_PLUS) OR (tok == MINUS_MINUS)) {

      syntax_error("++/-- not supported");

    } else {
      break;
    }
  }

  return result;
}

#ifndef SIX_CC
ast parse_cast_expression();
ast parse_compound_statement();
#endif

ast parse_unary_expression() {

  ast result;
  int op;

  if ((tok == PLUS_PLUS) OR (tok == MINUS_MINUS)) {

    op = tok;
    get_tok();
    result = parse_unary_expression();
    result = new_ast1(op, result);

  } else if ((tok == '&') OR (tok == '*') OR (tok == '+') OR (tok == '-') OR (tok == '~') OR (tok == '!')) {

    op = tok;
    get_tok();
    result = parse_cast_expression();
    result = new_ast1(op, result);

  } else if (tok == SIZEOF_KW) {

    syntax_error("sizeof not supported");

  } else {
    result = parse_postfix_expression();
  }

  return result;
}

ast parse_cast_expression() {
  return parse_unary_expression();
}

ast parse_multiplicative_expression() {

  ast result = parse_cast_expression();
  ast child;
  int op;

  while ((tok == '*') OR (tok == '/') OR (tok == '%')) {

    op = tok;
    get_tok();
    child = parse_cast_expression();
    result = new_ast2(op, result, child);

  }

  return result;
}

ast parse_additive_expression() {

  ast result = parse_multiplicative_expression();
  ast child;
  int op;

  while ((tok == '+') OR (tok == '-')) {

    op = tok;
    get_tok();
    child = parse_multiplicative_expression();
    result = new_ast2(op, result, child);

  }

  return result;
}

ast parse_shift_expression() {

  ast result = parse_additive_expression();
  ast child;
  int op;

  while ((tok == LSHIFT) OR (tok == RSHIFT)) {

    op = tok;
    get_tok();
    child = parse_additive_expression();
    result = new_ast2(op, result, child);

  }

  return result;
}

ast parse_relational_expression() {

  ast result = parse_shift_expression();
  ast child;
  int op;

  while ((tok == '<') OR (tok == '>') OR (tok == LT_EQ) OR (tok == GT_EQ)) {

    op = tok;
    get_tok();
    child = parse_shift_expression();
    result = new_ast2(op, result, child);

  }

  return result;
}

ast parse_equality_expression() {

  ast result = parse_relational_expression();
  ast child;
  int op;

  while ((tok == EQ_EQ) OR (tok == EXCL_EQ)) {

    op = tok;
    get_tok();
    child = parse_relational_expression();
    result = new_ast2(op, result, child);

  }

  return result;
}

ast parse_AND_expression() {

  ast result = parse_equality_expression();
  ast child;

  while (tok == '&') {

    get_tok();
    child = parse_equality_expression();
    result = new_ast2('&', result, child);

  }

  return result;
}

ast parse_exclusive_OR_expression() {

  ast result = parse_AND_expression();
  ast child;

  while (tok == '^') {

    get_tok();
    child = parse_AND_expression();
    result = new_ast2('^', result, child);

  }

  return result;
}


ast parse_inclusive_OR_expression() {

  ast result = parse_exclusive_OR_expression();
  ast child;

  while (tok == '|') {

    get_tok();
    child = parse_exclusive_OR_expression();
    result = new_ast2('|', result, child);

  }

  return result;
}


ast parse_logical_AND_expression() {

  ast result = parse_inclusive_OR_expression();
  ast child;

  while (tok == AMP_AMP) {

    get_tok();
    child = parse_inclusive_OR_expression();
    result = new_ast2(AMP_AMP, result, child);

  }

  return result;
}

ast parse_logical_OR_expression() {

  ast result = parse_logical_AND_expression();
  ast child;

  while (tok == BAR_BAR) {

    get_tok();
    child = parse_logical_AND_expression();
    result = new_ast2(BAR_BAR, result, child);

  }

  return result;
}

ast parse_conditional_expression() {

  ast result = parse_logical_OR_expression();
  ast child1;
  ast child2;

  if (tok == '?') {

    get_tok();
    child1 = parse_comma_expression();
    expect_tok(':');
    child2 = parse_conditional_expression();
    result = new_ast3('?', result, child1, child2);

  }

  return result;
}

ast parse_assignment_expression() {

  ast result = parse_conditional_expression();
  ast child;
  int op;

  if ((tok == '=') OR (tok == PLUS_EQ) OR (tok == MINUS_EQ) OR (tok == STAR_EQ) OR (tok == SLASH_EQ) OR (tok == PERCENT_EQ) OR (tok == LSHIFT_EQ) OR (tok == RSHIFT_EQ) OR (tok == AMP_EQ) OR (tok == CARET_EQ) OR (tok == BAR_EQ)) {

    op = tok;
    get_tok();
    child = parse_assignment_expression();
    result = new_ast2(op, result, child);

  }

  return result;
}

ast parse_comma_expression() {

  ast result = parse_assignment_expression();
  ast child;

  while (tok == ',') {

    get_tok();
    child = parse_assignment_expression();
    result = new_ast2(',', result, child);

  }

  return result;
}

ast parse_comma_expression_opt() {

  ast result;

  if ((tok == ':') OR (tok == ';') OR (tok == ')')) {
    result = 0;
  } else {
    result = parse_comma_expression();
  }

  return result;
}

ast parse_expression() {
  return parse_comma_expression();
}

ast parse_constant_expression() {
  return parse_expression();
}

ast parse_statement() {

  ast result;
  ast child1;
  ast child2;
  ast child3;
  int start_tok;

  if (tok == IF_KW) {

    get_tok();
    result = parse_parenthesized_expression();
    child1 = parse_statement();

    if (tok == ELSE_KW) {
      get_tok();
      child2 = parse_statement();
    } else {
      child2 = 0;
    }

    result = new_ast3(IF_KW, result, child1, child2);

  } else if (tok == SWITCH_KW) {

    get_tok();
    result = parse_parenthesized_expression();
    child1 = parse_statement();

    result = new_ast2(SWITCH_KW, result, child1);

  } else if (tok == CASE_KW) {

    get_tok();
    result = parse_constant_expression();
    expect_tok(':');
    child1 = parse_statement();

    result = new_ast2(CASE_KW, result, child1);

  } else if (tok == DEFAULT_KW) {

    get_tok();
    expect_tok(':');
    result = parse_statement();

    result = new_ast1(DEFAULT_KW, result);

  } else if (tok == WHILE_KW) {

    get_tok();
    result = parse_parenthesized_expression();
    child1 = parse_statement();

    result = new_ast2(WHILE_KW, result, child1);

  } else if (tok == DO_KW) {

    get_tok();
    result = parse_statement();
    expect_tok(WHILE_KW);
    child1 = parse_parenthesized_expression();
    expect_tok(';');

    result = new_ast2(DO_KW, result, child1);

  } else if (tok == FOR_KW) {

    get_tok();
    expect_tok('(');
    result = parse_comma_expression_opt();
    expect_tok(';');
    child1 = parse_comma_expression_opt();
    expect_tok(';');
    child2 = parse_comma_expression_opt();
    expect_tok(')');
    child3 = parse_statement();

    result = new_ast4(FOR_KW, result, child1, child2, child3);

  } else if (tok == GOTO_KW) {

    get_tok();
    expect_tok(IDENTIFIER);
    result = new_ast0(GOTO_KW, val);
    expect_tok(';');

  } else if (tok == CONTINUE_KW) {

    get_tok();
    expect_tok(';');

    result = new_ast0(CONTINUE_KW, 0);

  } else if (tok == BREAK_KW) {

    get_tok();
    expect_tok(';');

    result = new_ast0(BREAK_KW, 0);

  } else if (tok == RETURN_KW) {

    get_tok();
    result = parse_comma_expression_opt();
    expect_tok(';');

    result = new_ast1(RETURN_KW, result);

  } else if (tok == '{') {

    result = parse_compound_statement();

  } else {

    start_tok = tok;

    result = parse_comma_expression_opt();

    if ((tok == ':') AND (start_tok != '(') AND (get_op(result) == IDENTIFIER)) {

      child1 = parse_statement();

      result = new_ast2(':', result, child1);

    } else {

      expect_tok(';');

    }
  }

  return result;
}

ast parse_compound_statement() {

  ast result = 0;
  ast child1;

  expect_tok('{');

  while ((tok != '}') AND (tok != EOF)) {
    child1 = parse_statement();
    result = new_ast2('{', result, child1);
  }

  expect_tok('}');

  return result;
}



void print_string_char(int c) {
  if (c == 7) printf("\\a");
  else if (c == 8) printf("\\b");
  else if (c == 12) printf("\\f");
  else if (c == 10) printf("\\n");
  else if (c == 13) printf("\\r");
  else if (c == 9) printf("\\t");
  else if (c == 11) printf("\\v");
  else if ((c == '\\') OR (c == '\'') OR (c == '\"')) printf("\\%c", c);
  else if ((c < 32) OR (c > 126)) printf("\\%d%d%d", c>>6, (c>>3)&7, c&7);
  else putchar(c);
}

void print_tok(int tok, int val) {

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

  else if (tok == AMP_AMP) printf("&&");
  else if (tok == AMP_EQ) printf("&=");
  else if (tok == BAR_BAR) printf("||");
  else if (tok == BAR_EQ) printf("|=");
  else if (tok == CARET_EQ) printf("^=");
  else if (tok == EQ_EQ) printf("==");
  else if (tok == GT_EQ) printf(">=");
  else if (tok == LSHIFT_EQ) printf("<<=");
  else if (tok == LSHIFT) printf("<<");
  else if (tok == LT_EQ) printf("<=");
  else if (tok == MINUS_EQ) printf("-=");
  else if (tok == EXCL_EQ) printf("!=");
  else if (tok == PERCENT_EQ) printf("%%=");
  else if (tok == PLUS_EQ) printf("+=");
  else if (tok == RSHIFT_EQ) printf(">>=");
  else if (tok == RSHIFT) printf(">>");
  else if (tok == SLASH_EQ) printf("/=");
  else if (tok == STAR_EQ) printf("*=");

  else if (tok == IDENTIFIER) {
    printf("%s", string_pool + heap[val+1]);
  } else if (tok == INTEGER) {
    printf("%d", -val);
  } else if (tok == CHARACTER) {
    printf("'");
    print_string_char(val);
    printf("' ");
  } else if (tok == STRING) {
    printf("\"");
    i = 0;
    while (string_pool[val+i] != 0) {
      print_string_char(string_pool[val+i]);
      i += 1;
    }
    printf("\" ");
  } else {
    printf("%c", tok);
  }
}

/* codegen */

#define string_tree int
#define STRING_TREE_SIZE 100000
void_ptr string_tree_pool[STRING_TREE_SIZE];
int string_tree_alloc = 0;

/*
  Because concatenating strings is very expensive and a common operation, we
  use a tree structure to represent the concatenated strings. That way, the
  concatenation can be done in O(1).
  At the end of the codegen process, the tree will be flattened into a single
  string.
*/

string_tree wrap_str(char_ptr s) {
  string_tree_pool[string_tree_alloc] = STRING_TREE_STRING;
  string_tree_pool[string_tree_alloc + 1] = s;
  return (string_tree_alloc += 2) - 2;
}

string_tree wrap_str_pool(char_ptr s) {
  string_tree_pool[string_tree_alloc] = STRING_TREE_STRING_POOL;
  string_tree_pool[string_tree_alloc + 1] = s;
  return (string_tree_alloc += 2) - 2;
}

string_tree wrap_int(int i) {
  string_tree_pool[string_tree_alloc] = STRING_TREE_INTEGER;
  string_tree_pool[string_tree_alloc + 1] = i;
  return (string_tree_alloc += 2) - 2;
}

string_tree wrap_char(char c) {
  string_tree_pool[string_tree_alloc] = STRING_TREE_CHAR;
  string_tree_pool[string_tree_alloc + 1] = c;
  return (string_tree_alloc += 2) - 2;
}

string_tree string_concat(string_tree t1, string_tree t2) {
  string_tree_pool[string_tree_alloc] = STRING_TREE;
  string_tree_pool[string_tree_alloc + 1] = 2;
  string_tree_pool[string_tree_alloc + 2] = t1;
  string_tree_pool[string_tree_alloc + 3] = t2;
  return (string_tree_alloc += 4) - 4;
}

string_tree string_concat3(string_tree t1, string_tree t2, string_tree t3) {
  string_tree_pool[string_tree_alloc] = STRING_TREE;
  string_tree_pool[string_tree_alloc + 1] = 3;
  string_tree_pool[string_tree_alloc + 2] = t1;
  string_tree_pool[string_tree_alloc + 3] = t2;
  string_tree_pool[string_tree_alloc + 4] = t3;
  return (string_tree_alloc += 5) - 5;
}

string_tree string_concat4(string_tree t1, string_tree t2, string_tree t3, string_tree t4) {
  string_tree_pool[string_tree_alloc] = STRING_TREE;
  string_tree_pool[string_tree_alloc + 1] = 4;
  string_tree_pool[string_tree_alloc + 2] = t1;
  string_tree_pool[string_tree_alloc + 3] = t2;
  string_tree_pool[string_tree_alloc + 4] = t3;
  string_tree_pool[string_tree_alloc + 5] = t4;
  return (string_tree_alloc += 6) - 6;
}

void print_string_tree(string_tree t) {
  int i;

  if (string_tree_pool[t] == STRING_TREE) {
    for (i = 0; i < string_tree_pool[t + 1]; i++) {
      print_string_tree(string_tree_pool[t + i + 2]);
    }
  } else if (string_tree_pool[t] == STRING_TREE_STRING) {
    printf("%s", string_tree_pool[t + 1]);
  } else if (string_tree_pool[t] == STRING_TREE_STRING_POOL) {
    fatal_error("Not supported");
  } else if (string_tree_pool[t] == STRING_TREE_INTEGER) {
    printf("%d", string_tree_pool[t + 1]);
  } else if (string_tree_pool[t] == STRING_TREE_CHAR) {
    putchar(string_tree_pool[t + 1]);
  } else {
    fatal_error("unexpected string tree node");
  }
}

void codegen(ast node) {

  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int val;

  if (nb_children == 0) {
    if (op == IDENTIFIER) {
      val = get_val(node);
      print_tok(op, val);
    } else if (op == INTEGER) {
      val = get_val(node);
      print_tok(op, val);
    } else if (op == CHARACTER) {
      val = get_val(node);
      print_tok(op, val);
    } else if (op == STRING) {
      val = get_val(node);
      print_tok(op, val);
    } else {
      printf("op=%d %c", op, op);
      fatal_error("unexpected operator");
    }
  } else if (nb_children == 1) {
    if ((op == '&') OR (op == '*') OR (op == '+') OR (op == '-') OR (op == '~') OR (op == '!')) {
      printf("( ");
      print_tok(op, 0);
      codegen(get_child(node, 0));
      printf(" )");
    } else {
      printf("op=%d %c", op, op);
      fatal_error("unexpected operator");
    }
  } else if (nb_children == 2) {
      if ((op == '&') OR (op == '|') OR (op == '<') OR (op == '>') OR (op == '+') OR (op == '-') OR (op == '*') OR (op == '/') OR (op == '%') OR (op == '^') OR (op == ',') OR (op == AMP_AMP) OR (op == BAR_BAR) OR (op == LT_EQ) OR (op == GT_EQ) OR (op == EQ_EQ) OR (op == LSHIFT) OR (op == RSHIFT)) {
        printf("( ");
        codegen(get_child(node, 0));
        print_tok(op, 0);
        codegen(get_child(node, 1));
        printf(" )");
      } else if ((op == '=') OR (op == AMP_EQ) OR (op == BAR_EQ) OR (op == CARET_EQ) OR (op == LSHIFT_EQ) OR (op == MINUS_EQ) OR (op == PERCENT_EQ) OR (op == PLUS_EQ) OR (op == RSHIFT_EQ) OR (op == SLASH_EQ) OR (op == STAR_EQ)) {
        codegen(get_child(node, 0));
        print_tok(op, 0);
        codegen(get_child(node, 1));
        printf("; \n");
      } else {
        printf("op=%d %c", op, op);
        fatal_error("unexpected operator");
      }
  } else if (nb_children == 3) {
    printf("op=%d %c", op, op);
    fatal_error("unexpected operator");
  } else if (nb_children == 4) {
    printf("op=%d %c", op, op);
    fatal_error("unexpected operator");
  } else {
    printf("op=%d %c", op, op);
    fatal_error("unexpected operator");
  }
}

void codegen_statement(ast node) {
  codegen(node);
}

int main() {

  ast node;

  init_ident_table();

  ch = '\n';
  get_tok();

  while (tok != EOF) {
    node = parse_statement();
    /*    codegen_statement(node); */
  }
  /*
  printf("// string_pool_alloc=%d heap_alloc=%d\n", string_pool_alloc, heap_alloc);
  */
  return 0;
}
