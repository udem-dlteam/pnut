#ifndef PNUT_CC

#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>

#endif


#define ast int
#define true 1
#define false 0
#define EOF (-1)

#define AVOID_AMPAMP_BARBAR_not

#define OPTIMIZE_CONSTANT_PARAM_not
#define SUPPORT_ADDRESS_OF_OP_not
#define HANDLE_SIMPLE_PRINTF_not // Have a special case for printf("...") calls

#ifdef AVOID_AMPAMP_BARBAR
#define AND &
#define OR |
#else
#define AND &&
#define OR ||
#endif

#ifdef PNUT_CC

typedef int FILE;

/* Redefining strcmp because it's not part of the Shell runtime */
int strcmp(char *str1, char *str2) {
  int i = 0;
  while (str1[i] == str2[i]) {
    if (str1[i] == '\0') return 0;
    i += 1;
  }
  return str1[i] - str2[i];
}

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
int VAR_DECL       = 339;
int FUN_DECL       = 340;

int IDENTIFIER = 400;
int INTEGER    = 401;
int CHARACTER  = 402;
int STRING     = 403;

int AMP_AMP    = 404;
int AMP_EQ     = 405;
int ARROW      = 406;
int BAR_BAR    = 407;
int BAR_EQ     = 408;
int CARET_EQ   = 409;
int EQ_EQ      = 410;
int GT_EQ      = 411;
int LSHIFT_EQ  = 412;
int LSHIFT     = 413;
int LT_EQ      = 414;
int MINUS_EQ   = 415;
int MINUS_MINUS= 416;
int EXCL_EQ    = 417;
int PERCENT_EQ = 418;
int PLUS_EQ    = 419;
int PLUS_PLUS  = 420;
int RSHIFT_EQ  = 421;
int RSHIFT     = 422;
int SLASH_EQ   = 423;
int STAR_EQ    = 424;

int TEXT_TREE = 425;
int TEXT_INTEGER = 426;
int TEXT_CHAR = 427;
int TEXT_FROM_POOL = 428;

int IDENTIFIER_INTERNAL = 430;
int IDENTIFIER_STRING = 431;
int IDENTIFIER_DOLLAR = 432;
int IDENTIFIER_EMPTY = 433;
int LOCAL_VAR = 434;
int KIND_LOCAL = 435;
int KIND_PARAM = 436;

int TYPE = 437;

void fatal_error(char *msg) {
  printf("%s\n", msg);
  exit(1);
}

void print_dec(int n) {
  if (n < 0) {
    putchar('-');
    print_dec(-n);
  } else {
    if (n > 9) print_dec(n / 10);
    putchar('0' + n % 10);
  }
}

void print_hex(int n) {
  if (n < 0) {
    putchar('-');
    print_hex(-n);
  } else {
    if (n > 15) print_hex(n >> 4);
    putchar("0123456789abcdef"[n & 0xf]);
  }
}

/* tokenizer */

int ch;
int tok;
int val;

#define STRING_POOL_SIZE 50000
char string_pool[STRING_POOL_SIZE];
int string_pool_alloc = 0;
int string_start;
int hash;

/* These parameters give a perfect hashing of the C keywords */
#define HASH_PARAM 2764
#define HASH_PRIME 107
#define HEAP_SIZE 200000 /* MUST BE > HASH_PRIME */
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

  probe = alloc_obj(4);

  heap[hash] = probe; /* add new ident at end of chain */

  heap[probe] = 0; /* no next ident */
  heap[probe+1] = string_start;
  heap[probe+2] = IDENTIFIER;
  heap[probe+3] = 0; /* Token tag */

  return probe;
}

#ifndef PNUT_CC
void get_tok();
#endif

#define INCLUDE_DEPTH_MAX 5
FILE * include_stack[INCLUDE_DEPTH_MAX]; // Stack of file pointers that get_ch reads from
int include_stack_ptr = 0; // Points to the next available slot in the stack
FILE * fp; // Current file pointer that's being read

void get_ch() {
  ch = fgetc(fp);
  if (ch == EOF) {
    // If it's not the last file on the stack, EOF means that we need to switch to the next file
    if (include_stack_ptr > 1) {
      fclose(include_stack[include_stack_ptr-1]);
      include_stack_ptr -= 1;
      fp = include_stack[include_stack_ptr-1];
      get_ch();
    }
  }
}

void include_file(char *file_name) {
  if (include_stack_ptr >= INCLUDE_DEPTH_MAX) {
    fatal_error("Too many nested #include directives. Maximum supported is 5.");
  }
  fp = fopen(file_name, "r");
  include_stack[include_stack_ptr] = fp;
  include_stack_ptr += 1;
}

void handle_preprocessor_directive() {
  get_ch();
  get_tok();
  if (tok == INCLUDE_KW) {
    get_tok();
    if (tok == STRING) {
      include_file(string_pool + val);
    } else {
      fatal_error("expected string to #include directive");
    }
  } else {
    while ((ch != '\n') AND (ch != EOF)) {
      printf("%c", ch);
      get_ch();
    }
  }
}

void get_ident() {

  begin_string();

  while (('A' <= ch AND ch <= 'Z') OR
         ('a' <= ch AND ch <= 'z') OR
         ('0' <= ch AND ch <= '9') OR
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

int init_ident(int tok, char *name) {

  int i = 0;

  begin_string();

  while (name[i] != 0) {
    ch = name[i];
    accum_string();
    i += 1;
  }

  i = end_ident();

  heap[i+2] = tok;

  return i;
}

void init_ident_table() {

  int i = 0;

  while (i < HASH_PRIME) {
    heap[i] = 0;
    i += 1;
  }

  init_ident(AUTO_KW,     "auto");
  init_ident(BREAK_KW,    "break");
  init_ident(CASE_KW,     "case");
  init_ident(CHAR_KW,     "char");
  init_ident(CONST_KW,    "const");
  init_ident(CONTINUE_KW, "continue");
  init_ident(DEFAULT_KW,  "default");
  init_ident(DEFINE_KW,   "define");
  init_ident(DO_KW,       "do");
  init_ident(DOUBLE_KW,   "double");
  init_ident(ELSE_KW,     "else");
  init_ident(ENDIF_KW,    "endif");
  init_ident(ENUM_KW,     "enum");
  init_ident(ERROR_KW,    "error");
  init_ident(EXTERN_KW,   "extern");
  init_ident(FLOAT_KW,    "float");
  init_ident(FOR_KW,      "for");
  init_ident(GOTO_KW,     "goto");
  init_ident(IF_KW,       "if");
  init_ident(IFDEF_KW,    "ifdef");
  init_ident(IFNDEF_KW,   "ifndef");
  init_ident(INCLUDE_KW,  "include");
  init_ident(INT_KW,      "int");
  init_ident(LONG_KW,     "long");
  init_ident(REGISTER_KW, "register");
  init_ident(RETURN_KW,   "return");
  init_ident(SHORT_KW,    "short");
  init_ident(SIGNED_KW,   "signed");
  init_ident(SIZEOF_KW,   "sizeof");
  init_ident(STATIC_KW,   "static");
  init_ident(STRUCT_KW,   "struct");
  init_ident(SWITCH_KW,   "switch");
  init_ident(TYPEDEF_KW,  "typedef");
  init_ident(UNDEF_KW,    "undef");
  init_ident(UNION_KW,    "union");
  init_ident(UNSIGNED_KW, "unsigned");
  init_ident(VOID_KW,     "void");
  init_ident(VOLATILE_KW, "volatile");
  init_ident(WHILE_KW,    "while");
}

int accum_digit(int base) {
  int digit = 99;
  if ('0' <= ch AND ch <= '9') {
    digit = ch - '0';
  } else if ('A' <= ch AND ch <= 'Z') {
    digit = ch - 'A' + 10;
  } else if ('a' <= ch AND ch <= 'z') {
    digit = ch - 'a' + 10;
  }
  if (digit >= base) {
    return 0; /* character is not a digit in that base */
  } else {
    /*
    TODO: Put overflow check back
    if ((val < limit) OR ((val == limit) AND (digit > limit * base - MININT))) {
      fatal_error("literal integer overflow");
    }
    */
    val = val * base - digit;
    get_ch();
    return 1;
  }
}

void get_string_char() {

  val = ch;
  get_ch();

  if (val == '\\') {
    if ('0' <= ch AND ch <= '7') {
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

      while (0 <= ch AND ch <= ' ') {
        if (ch == '\n') tok = ch;
        get_ch();
      }

      /* detect '#' at start of line, possibly preceded by whitespace */

      if ((tok == '\n') AND (ch == '#'))
        handle_preprocessor_directive();

      /* will continue while (1) loop */

    }

    else if (('a' <= ch AND ch <= 'z') OR
               ('A' <= ch AND ch <= 'Z') OR
               (ch == '_')) {

      get_ident();

      break;

    } else if ('0' <= ch AND ch <= '9') {

      val = '0' - ch;

      get_ch();

      if (val == 0) { /* val == 0 <=> ch == '0' */
        if ((ch == 'x') OR (ch == 'X')) {
          get_ch();
          val = 0;
          if (accum_digit(16)) {
            while (accum_digit(16));
          } else {
            fatal_error("invalid hex integer -- it must have at least one digit");
          }
        } else {
          while (accum_digit(8));
        }
      } else {
        while (accum_digit(10));
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

  ast_result = alloc_obj(4);

  heap[ast_result] = op + 3072;
  set_child(ast_result, 0, child0);
  set_child(ast_result, 1, child1);
  set_child(ast_result, 2, child2);

  return ast_result;
}

ast new_ast4(int op, ast child0, ast child1, ast child2, ast child3) {

  ast_result = alloc_obj(5);

  heap[ast_result] = op + 4096;
  set_child(ast_result, 0, child0);
  set_child(ast_result, 1, child1);
  set_child(ast_result, 2, child2);
  set_child(ast_result, 3, child3);

  return ast_result;
}

void syntax_error(char *msg) {
  printf("syntax error: %s\n", msg);
  fatal_error("syntax error");
}

void missing_feature_error(char *msg) {
  printf("not yet implemented: %s\n", msg);
  fatal_error("syntax error");
}

void expect_tok(int expected_tok) {
  if (tok != expected_tok) {
    printf("expected_tok=%d tok=%d\n", expected_tok, tok);
    syntax_error("unexpected token");
  }
  get_tok();
}

#ifndef PNUT_CC
ast parse_comma_expression();
ast parse_cast_expression();
ast parse_compound_statement();
ast parse_conditional_expression();
#endif

ast parse_type() {

  int type_kw = 0;

  while (1) {
    if ((tok == INT_KW) OR (tok == CHAR_KW) OR (tok == SHORT_KW) OR (tok == LONG_KW) OR (tok == SIGNED_KW)) {
      if ((type_kw != 0) AND (type_kw != INT_KW)) {
        syntax_error("inconsistent type");
      } else {
        type_kw = INT_KW;
        get_tok();
      }
    } else if ((tok == UNSIGNED_KW) OR (tok == FLOAT_KW) OR (tok == DOUBLE_KW)) {
      syntax_error("unsupported type");
    } else if (tok == VOID_KW) {
      if (type_kw != 0) {
        syntax_error("inconsistent type");
      } else {
        type_kw = VOID_KW;
        get_tok();
      }
    } else if (tok == CONST_KW) {
      get_tok(); // ignore const
    } else if (tok == TYPE) {
      /* Look in types table */
      type_kw = heap[val + 3]; /* For TYPE tokens, the tag is the type */
      get_tok();
      break;
    } else {
      break;
    }
  }

  if (type_kw == 0) {
    syntax_error("type expected");
  }

  return new_ast0(type_kw, 0);
}

int parse_stars() {

  int stars = 0;

  while (tok == '*') {
    stars += 1;
    get_tok();
  }

  return stars;
}

int is_type_starter(int tok) {
  return (tok == INT_KW) OR (tok == CHAR_KW) OR (tok == SHORT_KW) OR (tok == LONG_KW) OR (tok == SIGNED_KW) // Supported types
      OR (tok == UNSIGNED_KW) OR (tok == FLOAT_KW) OR (tok == DOUBLE_KW) OR (tok == VOID_KW) // Unsupported types
      OR (tok == TYPE) // User defined types
      OR (tok == CONST_KW); // Type attributes
}

ast parse_declaration() {

  ast type;
  int stars;
  int name;
  ast result = 0;

  if (is_type_starter(tok)) {

    type = parse_type();
    stars = parse_stars();

    set_val(type, stars);

    name = val;

    expect_tok(IDENTIFIER);

    if ((stars == 0) AND (get_op(type) == VOID_KW))
      syntax_error("variable with void type");
    /*
    if (tok == '[')
      syntax_error("array declaration only allowed at global level");
    */

    result = new_ast3(VAR_DECL, name, type, 0);
  }

  return result;
}

int parse_declaration_list() {
  ast decl = parse_declaration();
  ast result = 0;
  ast tail;
  if (decl != 0) {
    result = new_ast2(',', decl, 0);
    tail = result;

    while (tok == ',') {
      get_tok();
      decl = parse_declaration();
      if (decl == 0) { break; }

      decl = new_ast2(',', decl, 0);
      set_child(tail, 1, decl);
      tail = decl;
    }
  }

  return result;
}

/* Note: Uses a simplified syntax for definitions */
ast parse_definition(int local) {

  ast type;
  int stars;
  ast init;
  int name;
  ast params;
  ast body;
  ast this_type;
  ast result = 0;

  if (is_type_starter(tok)) {

    type = parse_type();

    while (1) {

      stars = parse_stars();

      this_type = type;
      if (stars != 0) {
        this_type = new_ast0(get_op(type), stars);
      }

      name = val;

      expect_tok(IDENTIFIER);

      if (tok == '(') {

        if (local) {
          syntax_error("function declaration only allowed at global level");
        }

        get_tok();

        params = parse_declaration_list();

        expect_tok(')');

        if (tok == ';') {
          /* forward declaration */
          body = 0;
          get_tok();
        } else {
          body = parse_compound_statement();
        }

        return new_ast4(FUN_DECL, name, this_type, params, body);

      } else {

        if ((stars == 0) AND (get_op(type) == VOID_KW)) {
          syntax_error("variable with void type");
        }

        if (tok == '[') {
          /*
          if (local) {
            syntax_error("array declaration only allowed at global level");
          }
          */
          get_tok();
          if (tok == INTEGER) {
            this_type = new_ast2('[', new_ast0(INTEGER, -val), this_type);
            get_tok();
          } else {
            syntax_error("array size must be an integer constant");
          }

          expect_tok(']');
        }

        init = 0;

        if (tok == '=') {
          get_tok();
          init = parse_conditional_expression();
        }

        result = new_ast3(VAR_DECL, name, this_type, init);

        if (tok == ';') {
          get_tok();
          break;
        } else if (tok == ',') {
          get_tok();
          break;
        } else {
          syntax_error("';' or ',' expected");
        }
      }
    }
    return result;
  } else if (tok == TYPEDEF_KW) {
    // When parsing a typedef, the type is added to the types table.
    // Since the code generators don't do anything with typedefs, we then return
    // the next definition.
    get_tok();
    type = parse_type();
    if (tok != IDENTIFIER) { syntax_error("identifier expected"); }

    heap[val + 2] = TYPE;
    heap[val + 3] = type;
    get_tok();
    expect_tok(';');
    return parse_definition(local);
  } else {
    return result;
  }
}

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
  ast tail;

  if (tok == ',') { /* List of 1 elements are not boxed in a cons cell */
    result = new_ast2(',', result, 0); /* Create default cons cell */
    tail = result;
    while (tok == ',') {

      get_tok();
      child = parse_assignment_expression();
      child = new_ast2(',', child, 0); /* New tail cons cell */
      set_child(tail, 1, child);       /* Add new cons cell at end of list */
      tail = child;                    /* Advance tail */

    }
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
  ast tail;

  expect_tok('{');

  /* TODO: Simplify this */
  if ((tok != '}') AND (tok != EOF)) {
    if (is_type_starter(tok)) {
      child1 = parse_definition(1);
    } else {
      child1 = parse_statement();
    }
    result = new_ast2('{', child1, 0);
    tail = result;
    while ((tok != '}') AND (tok != EOF)) {
      if (is_type_starter(tok)) {
        child1 = parse_definition(1);
      } else {
        child1 = parse_statement();
      }
      child1 = new_ast2('{', child1, 0);
      set_child(tail, 1, child1);
      tail = child1;
    }
  }

  expect_tok('}');

  return result;
}

//-----------------------------------------------------------------------------

// Select code generator

#ifdef sh
#include "sh.c"
#endif

#ifdef i386
#include "x86.c"
#endif

#ifdef x86_64
#include "x86.c"
#endif

#ifdef arm
#include "arm.c"
#endif

//-----------------------------------------------------------------------------

int main(int argc, char **args) {

  if (argc == 2) {
    include_file(args[1]);
  } else {
    printf("Usage: %s <filename>\n", args[0]);
    return 1;
  }

  init_ident_table();

  codegen_begin();

  ch = '\n';
  get_tok();

  while (tok != EOF) {
    codegen_glo_decl(parse_definition(0));
  }

  codegen_end();

  return 0;
}
