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

// At the moment of adding this compile option, the x86 runtime library doesn't
// support fopen and fgetc, meaning that #include directives can't be used.
#define SUPPORT_INCLUDE_not

#define AVOID_AMPAMP_BARBAR_not

#define OPTIMIZE_CONSTANT_PARAM_not
#define SUPPORT_ADDRESS_OF_OP_not
#define HANDLE_SIMPLE_PRINTF_not

#ifdef AVOID_AMPAMP_BARBAR
#define AND &
#define OR |
#else
#define AND &&
#define OR ||
#endif

typedef int bool;

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
int DO_KW          = 307;
int DOUBLE_KW      = 308;
int ELSE_KW        = 309;
int ENUM_KW        = 310;
int ERROR_KW       = 311;
int EXTERN_KW      = 312;
int FLOAT_KW       = 313;
int FOR_KW         = 314;
int GOTO_KW        = 315;
int IF_KW          = 316;
int IFNDEF_KW      = 317;
int INCLUDE_KW     = 318;
int INT_KW         = 319;
int LONG_KW        = 320;
int REGISTER_KW    = 321;
int RETURN_KW      = 322;
int SHORT_KW       = 323;
int SIGNED_KW      = 324;
int SIZEOF_KW      = 325;
int STATIC_KW      = 326;
int STRUCT_KW      = 327;
int SWITCH_KW      = 328;
int TYPEDEF_KW     = 329;
int UNION_KW       = 330;
int UNSIGNED_KW    = 331;
int VOID_KW        = 332;
int VOLATILE_KW    = 333;
int WHILE_KW       = 334;
int VAR_DECL       = 335;
int FUN_DECL       = 336;

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
int HASH_HASH  = 425;

//pre and post increment and decrement
int PLUS_PLUS_PRE = 425;
int MINUS_MINUS_PRE = 426;
int PLUS_PLUS_POST = 427;
int MINUS_MINUS_POST = 428;

int MACRO_ARG = 499;
int IDENTIFIER = 500;
int TYPE = 501;
int MACRO = 502;

void putstr(char *str) {
  while (*str) {
    putchar(*str);
    str += 1;
  }
}

void putint(int n) {
  if (n < 0) {
    putchar('-');
    putint(-n);
  } else {
    if (n > 9) putint(n / 10);
    putchar('0' + n % 10);
  }
}

void fatal_error(char *msg) {
  putstr(msg); putchar('\n');
  exit(1);
}

void syntax_error(char *msg) {
  putstr("syntax error: "); putstr(msg);
  fatal_error("syntax error");
}

void missing_feature_error(char *msg) {
  putstr("not yet implemented: "); putstr(msg);
  fatal_error("syntax error");
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
#define HEAP_SIZE 200000
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

int cons(int child0, int child1) {

  int result = alloc_obj(2);

  heap[result] = child0;
  heap[result+1] = child1;

  return result;
}

int car(int pair) {
  return heap[pair];
}

int cdr(int pair) {
  return heap[pair+1];
}

int set_car(int pair, int value) {
  heap[pair] = value;
  return value;
}

int set_cdr(int pair, int value) {
  heap[pair+1] = value;
  return value;
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

// Like accum_string, but takes the character as input instead of reading it from ch
void accum_string_char(char c) {
  hash = (c + (hash ^ HASH_PARAM)) % HASH_PRIME;
  string_pool[string_pool_alloc] = c;
  string_pool_alloc += 1;
  if (string_pool_alloc >= STRING_POOL_SIZE) {
    fatal_error("string pool overflow");
  }
}

// Like accum_string, but takes a string from the string_pool as input instead of reading it from ch
void accum_string_string(int s) {
  int i = 0;
  while (string_pool[s + i] != 0) {
    accum_string_char(string_pool[s + i]);
    i += 1;
  }
}

// Similar to accum_string_string, but writes an integer to the string pool
void accum_string_integer(int n) {
  if (n < 0) {
    accum_string_char('-');
    accum_string_integer(-n);
  } else {
    if (n > 9) accum_string_integer(n / 10);
    accum_string_char('0' + n % 10);
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
void get_ident();
void expect_tok(int expected);
#endif

#ifdef SUPPORT_INCLUDE
#define INCLUDE_DEPTH_MAX 5
FILE *include_stack[INCLUDE_DEPTH_MAX]; // Stack of file pointers that get_ch reads from
int include_stack_ptr = 0; // Points to the next available slot in the stack
FILE *fp = 0; // Current file pointer that's being read
#endif

#define IFDEF_DEPTH_MAX 20
bool ifdef_stack[IFDEF_DEPTH_MAX]; // Stack of ifdef states
bool ifdef_stack_ix = 0;
bool ifdef_mask = true;
// Whether to expand macros or not. Useful to parse macro definitions containing
// other macros without expanding them.
bool expand_macro = true;
// Don't expand macro arguments. Used for stringification and token pasting.
bool expand_macro_arg = true;

#define MACRO_RECURSION_MAX 100
int macro_stack[MACRO_RECURSION_MAX];
int macro_stack_ix = 0;

int macro_tok_lst = 0;  // Current list of tokens to replay for the macro being expanded
int macro_args = 0;     // Current list of arguments for the macro being expanded
int macro_args_count;   // Number of arguments for the current macro being expanded
bool paste_last_token = false; // Whether the last token was a ## or not

void flip_ifdef_mask() {
  ifdef_mask = !ifdef_mask;
}

void push_ifdef_mask(bool new_mask) {
  if (ifdef_stack_ix >= IFDEF_DEPTH_MAX) {
    fatal_error("Too many nested #ifdef/#ifndef directives. Maximum supported is 20.");
  }
  // Save current mask on the stack because it's about to be overwritten
  ifdef_stack[ifdef_stack_ix] = ifdef_mask;
  ifdef_stack_ix += 1;
  // Then set the new mask value
  ifdef_mask = new_mask;
}

void pop_ifdef_mask() {
  if (ifdef_stack_ix == 0) {
    fatal_error("Unbalanced #ifdef/#ifndef/#else/#endif directives.");
  }
  ifdef_stack_ix -= 1;
  ifdef_mask = ifdef_stack[ifdef_stack_ix];
}

void get_ch() {
#ifdef SUPPORT_INCLUDE
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
#else
  ch = getchar();
#endif
}

#ifdef SUPPORT_INCLUDE
void include_file(char *file_name) {
  if (include_stack_ptr >= INCLUDE_DEPTH_MAX) {
    fatal_error("Too many nested #include directives. Maximum supported is 5.");
  }
  fp = fopen(file_name, "r");
  include_stack[include_stack_ptr] = fp;
  include_stack_ptr += 1;
}
#endif

// We add the preprocessor keywords to the ident table so they can be easily
// recognized by the preprocessor. Because these are not C keywords, their kind
// is still IDENTIFIER so the parser (which runs after the preprocessor) can
// treat them as such.
int IFDEF_ID;
int IFNDEF_ID;
int ENDIF_ID;
int DEFINE_ID;
int UNDEF_ID;
int INCLUDE_ID;

int NOT_SUPPORTED_ID;

void get_tok_macro() {
  expand_macro = false;
  get_tok();
  expand_macro = true; // TODO: Restore to previous value?
}

int lookup_macro_token(int args, int tok, int val) {
  int ix = 0;

  if (tok < IDENTIFIER) return cons(tok, val); // Not an identifier

  while (args != 0) {
    if (car(args) == val) break; // Found!
    args = cdr(args);
    ix += 1;
  }

  if (args == 0) { // Identifier is not a macro argument
    return cons(tok, val);
  } else {
    return cons(MACRO_ARG, ix);
  }
}

int read_macro_tokens(int args) {
  int toks = 0; // List of token to replay
  int tail;

  // Accumulate tokens so they can be replayed when the macro is used
  if (ch != '\n' AND ch != EOF) {
    get_tok_macro();
    // Append the token/value pair to the replay list
    toks = cons(lookup_macro_token(args, tok, val), 0);
    tail = toks;
    while (ch != '\n' AND ch != EOF) {
      get_tok_macro();
      heap[tail + 1] = cons(lookup_macro_token(args, tok, val), 0);
      tail = cdr(tail); // Advance tail
    }

    // Check that there are no leading or trailing ##
    if (car(car(toks)) == HASH_HASH OR car(car(tail)) == HASH_HASH) {
      fatal_error("'##' cannot appear at either end of a macro expansion");
    }
  }

  return toks;
}

#ifdef DEBUG_CPP
void print_macro_raw_tokens(int tokens) {
  int i = 0;
  while (tokens != 0) {
    // print_tok(car(car(tokens)), cdr(car(tokens)));
    putchar(car(car(tokens))); putchar('('); putint(car(car(tokens))); putchar(')');
    tokens = cdr(tokens);
    i += 1;
  }
  putstr("("); putint(i); putstr(" tokens)");
}
#endif

// A few things that are different from the standard:
// - We allow sequence of commas in the argument list
// - Function-like macros with 0 arguments can be called either without parenthesis or with ().
// - No support for variadic macros. Tcc only uses them in tests so it should be ok.
void handle_define() {
  int macro;    // The identifier that is being defined as a macro
  int args = 0; // List of arguments for a function-like macro
  int args_count = -1; // Number of arguments for a function-like macro. -1 means it's an object-like macro

  get_tok_macro();
  if (tok == IDENTIFIER OR tok == MACRO) {
    heap[val + 2] = MACRO; // Mark the identifier as a macro
    macro = val;
  } else {
    putstr("tok="); putint(tok); putchar('\n');
    fatal_error("#define directive can only be followed by a identifier");
  }
  if (ch == '(') { // Function-like macro
    args_count = 0;
    get_ch();
    while (ch != '\n' AND ch != EOF) {
      if (ch == ',') {
        // Allow sequence of commas, this is more lenient than the standard
        get_ch();
        continue;
      } else if (ch == ')') {
        get_ch();
        break;
      }
      get_tok_macro();
      // Accumulate parameters in reverse order. That's ok because the arguments
      // to the macro will also be in reverse order.
      args = cons(val, args);
      args_count += 1;
    }
  }

  // Skip whitespace between the parameters and macro body
  while (ch != '\n' AND ch != EOF AND ch <= ' ') {
    get_ch();
  }

  if (ch == '\n' OR ch == EOF) {
    heap[macro + 3] = cons(0, args_count); // No tokens to replay
  } else {
    // Accumulate tokens so they can be replayed when the macro is used
    heap[macro + 3] = cons(read_macro_tokens(args), args_count);

    #ifdef DEBUG_CPP
    putstr("# ");
    putstr(string_pool + heap[macro + 1]);
    if (args_count != -1) putchar('('); // Function-like macro

    while (args_count > 0) {
      putstr(string_pool + heap[car(args) + 1]);
      args = cdr(args);
      args_count -= 1;
      if (args_count > 0) putstr(", ");
    }

    if (args_count != -1) putstr(") ");
    print_macro_raw_tokens(car(heap[macro + 3]));
    putchar('\n');
    #endif
  }
}

void handle_preprocessor_directive() {
  bool prev_ifdef_mask = ifdef_mask;
  get_ch(); // Skip the #
  ifdef_mask = true; // Temporarily set to true so that we can read the directive even if it's inside an ifdef false block
  get_tok(); // Get the directive
  ifdef_mask = prev_ifdef_mask;

  if (tok == IDENTIFIER AND val == ENDIF_ID) {
    pop_ifdef_mask();
  } else if (tok == ELSE_KW) {
    flip_ifdef_mask();
  } else if (ifdef_mask) {
    if (tok == IDENTIFIER AND val == IFDEF_ID) {
      get_tok_macro();
      push_ifdef_mask(tok == MACRO);
    } else if (tok == IDENTIFIER AND val == IFNDEF_ID) {
      get_tok_macro();
      push_ifdef_mask(tok != MACRO);
    } else if (tok == IDENTIFIER AND val == INCLUDE_ID) {
      get_tok();
      if (tok == STRING) {
        #ifdef SUPPORT_INCLUDE
        include_file(string_pool + val);
        #else
        fatal_error("The #include directive is not supported in this version of the compiler.");
        #endif
      } else {
        putstr("tok="); putint(tok); putchar('\n');
        fatal_error("expected string to #include directive");
      }
    } else if (tok == IDENTIFIER AND val == UNDEF_ID) {
      get_tok_macro();
      if (tok == MACRO) {
        heap[val + 2] = IDENTIFIER; // Unmark the macro identifier
        // TODO: Doesn't play nice with typedefs, because they are not marked as macros
      } else {
        putstr("tok="); putint(tok); putchar('\n');
        fatal_error("#undef directive can only be followed by a identifier");
      }
    } else if (tok == IDENTIFIER AND val == DEFINE_ID) {
      handle_define();
    } else {
      putstr("tok="); putint(tok); putstr(": "); putstr(string_pool + heap[val + 1]); putchar('\n');
      fatal_error("unsupported preprocessor directive");
    }
  } else {
    // Skip the directive
    while (ch != '\n' AND ch != EOF) {
      get_ch();
    }
  }
  // Because handle_preprocessor_directive is called from get_tok, and it loops after
  // the call to handle_preprocessor_directive, we don't need to call get_tok here
  if (ch != '\n' AND ch != EOF) {
    putstr("ch="); putint(ch); putchar('\n');
    fatal_error("preprocessor expected end of line");
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
  putstr("tok="); putint(tok);
  putstr(" val="); putint(val);
  putstr(" "); putstr(string_pool + heap[val+1]);
  putchar('\n');
  */
}

int init_ident(int tok, char *name) {

  int i = 0;
  int prev_ch = ch; // The character may be important to the calling function, saving it

  begin_string();

  while (name[i] != 0) {
    ch = name[i];
    accum_string();
    i += 1;
  }

  i = end_ident();

  heap[i+2] = tok;

  ch = prev_ch;

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
  init_ident(DO_KW,       "do");
  init_ident(DOUBLE_KW,   "double");
  init_ident(ELSE_KW,     "else");
  init_ident(ENUM_KW,     "enum");
  init_ident(ERROR_KW,    "error");
  init_ident(EXTERN_KW,   "extern");
  init_ident(FLOAT_KW,    "float");
  init_ident(FOR_KW,      "for");
  init_ident(GOTO_KW,     "goto");
  init_ident(IF_KW,       "if");
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
  init_ident(UNION_KW,    "union");
  init_ident(UNSIGNED_KW, "unsigned");
  init_ident(VOID_KW,     "void");
  init_ident(VOLATILE_KW, "volatile");
  init_ident(WHILE_KW,    "while");

  // Preprocessor keywords. These are not tagged as keyword since they can be
  // used as identifiers after the preprocessor stage.
  IFDEF_ID   = init_ident(IDENTIFIER, "ifdef");
  IFNDEF_ID  = init_ident(IDENTIFIER, "ifndef");
  ENDIF_ID   = init_ident(IDENTIFIER, "endif");
  DEFINE_ID  = init_ident(IDENTIFIER, "define");
  UNDEF_ID   = init_ident(IDENTIFIER, "undef");
  INCLUDE_ID = init_ident(IDENTIFIER, "include");

  // Stringizing is recognized by the macro expander, but it returns a hardcoded
  // string instead of the actual value. This may be enough to compile TCC.
  NOT_SUPPORTED_ID = init_ident(IDENTIFIER, "NOT_SUPPORTED");
}

void init_pnut_macros() {
  init_ident(MACRO, "PNUT_CC");
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

// A macro argument is represented using a list of tokens.
// Macro arguments are split by commas, but commas can also appear in function
// calls and as operators. To distinguish between the two, we need to keep track
// of the parenthesis depth.
int macro_parse_argument() {
  int arg_tokens = 0;
  int parens_depth = 0;
  int tail;

  while ((parens_depth > 0 OR (tok != ',' AND tok != ')')) AND tok != EOF) {
    if (tok == '(') parens_depth += 1; // Enter parenthesis
    if (tok == ')') parens_depth -= 1; // End of parenthesis

    if (arg_tokens == 0) {
      arg_tokens = cons(cons(tok, val), 0);
      tail = arg_tokens;
    } else {
      heap[tail + 1] = cons(cons(tok, val), 0);
      tail = cdr(tail);
    }
    get_tok_macro();
  }

  return arg_tokens;
}

void check_macro_arity(int macro_args_count, int expected_argc) {
  if (macro_args_count != expected_argc) {
    putstr("expected_argc="); putint(expected_argc);
    putstr(" != macro_args_count="); putint(macro_args_count);
    putchar('\n');
    fatal_error("macro argument count mismatch");
  }
}

// Reads the arguments of a macro call, where the arguments are split by commas.
// Note that args are accumulated in reverse order, as the macro arguments refer
// to the tokens in reverse order.
int get_macro_args_toks(int expected_argc) {
  int args = 0;
  int macro_args_count = 0;
  get_tok_macro(); // Skip the macro identifier

  if (tok != '(') { // Function-like macro with 0 arguments
    check_macro_arity(macro_args_count, expected_argc);
    return -1; // No arguments
  }

  get_tok_macro(); // Skip '('

  while (tok != ')' AND tok != EOF) {
    // Allow sequence of commas, this is more lenient than the standard
    if (tok == ',') {
      get_tok_macro(); // Skip comma
      continue;
    }
    args = cons(macro_parse_argument(), args);
    macro_args_count += 1;
  }

  expect_tok(')');

  check_macro_arity(macro_args_count, expected_argc);

  return args;
}

int get_macro_arg(int ix) {
  int arg = macro_args;
  while (ix > 0) {
    if (arg == 0) fatal_error("too few arguments to macro");
    arg = cdr(arg);
    ix -= 1;
  }
  return car(arg);
}

void push_macro(int tokens, int args) {
  if (tokens != 0) {
    if (macro_tok_lst != 0) {
      if (macro_stack_ix + 2 >= MACRO_RECURSION_MAX) {
        fatal_error("Macro recursion depth exceeded.");
      }
      macro_stack[macro_stack_ix] = macro_tok_lst;
      macro_stack[macro_stack_ix + 1] = macro_args;
      macro_stack_ix += 2;
    } else {
    }
    macro_tok_lst = tokens;
    macro_args = args;
  }
}

// Try to expand a macro.
// If a function-like macro is not called with (), it is not expanded and the identifier is returned as is.
// If the wrong number of arguments is passed to a function-like macro, a fatal error is raised.
// For object like macros, the macro tokens are played back without any other parsing.
// Returns 1 if the macro was expanded, 0 otherwise.
bool attempt_macro_expansion(int macro) {
  int new_macro_args;
  macro = val;
  if (cdr(heap[macro + 3]) == -1) { // Object-like macro
    push_macro(car(heap[macro + 3]), 0);
    return true;
  } else {
    new_macro_args = get_macro_args_toks(cdr(heap[macro + 3]));
    // get_macro_args_toks fetched the next token, we save it so it's not lost
    push_macro(cons(cons(tok, val), 0), new_macro_args);
    if (new_macro_args == -1) { // There was no argument list, i.e. not a function-like macro call
      // Function-like macro without (), so we don't expand it.
      tok = IDENTIFIER;
      val = macro;
      return false;
    } else {
      push_macro(car(heap[macro + 3]), new_macro_args);
      return true;
    }
  }
}

// https://gcc.gnu.org/onlinedocs/cpp/Stringizing.html
void stringify() {
  int arg;
  expand_macro_arg = false;
  get_tok_macro();
  expand_macro_arg = true;
  if (tok != MACRO_ARG) {
    putstr("tok="); putint(tok); putchar('\n');
    fatal_error("expected macro argument after #");
  }
  arg = get_macro_arg(val);
  tok = STRING;
  // Support the case where the argument is a single identifier token
  if (car(car(arg)) == IDENTIFIER AND cdr(arg) == 0) {
    val = heap[cdr(car(arg)) + 1]; // Use the identifier value
  } else {
    val = heap[NOT_SUPPORTED_ID + 1]; // Return string "NOT_SUPPORTED"
  }
}

int paste_integers(int left_val, int right_val) {
  int result = left_val;
  int right_digits = right_val;
  while (right_digits > 0) {
    result *= 10;
    right_digits /= 10;
  }
  return result + right_val;
}

// Support token pasting between identifiers and non-negative integers
void paste_tokens(int left_tok, int left_val) {
  int right_tok;
  int right_val;
  get_tok_macro();
  right_tok = tok;
  right_val = val;
  if (left_tok == IDENTIFIER OR left_tok == MACRO) {
    // Something that starts with an identifier can only be an identifier
    begin_string();
    accum_string_string(heap[left_val + 1]);

    if (right_tok == IDENTIFIER OR right_tok == MACRO) {
      accum_string_string(heap[right_val + 1]);
    } else if (right_tok == INTEGER) {
      accum_string_integer(-right_val);
    } else {
      putstr("left_tok="); putint(left_tok); putstr(", right_tok="); putint(right_tok); putchar('\n');
      fatal_error("cannot paste an identifier with a non-identifier or non-negative integer");
    }

    val = end_ident();
    tok = heap[val+2]; // The kind of the identifier
  } else if (left_tok == INTEGER) {
    if (right_tok == INTEGER) {
      val = -paste_integers(-left_val, -right_val);
    } else {
      putstr("left_tok="); putint(left_tok); putstr(", right_tok="); putint(right_tok); putchar('\n');
      fatal_error("cannot paste an integer with a non-integer");
    }
  } else {
    putstr("left_tok="); putint(left_tok); putstr(", right_tok="); putint(right_tok); putchar('\n');
    fatal_error("cannot paste a non-identifier or non-integer");
  }
}

void get_tok() {

  bool first_time = true; // Used to simulate a do-while loop

  // This outer loop is used to skip over tokens removed by #ifdef/#ifndef/#else
  while (first_time OR !ifdef_mask) {
    first_time = false;
    while (1) {
      // Check if there are any tokens to replay. Macros are just identifiers that
      // have been marked as macros. In terms of how we get into that state, a
      // macro token is first returned by the get_ident call a few lines below.
      if (macro_tok_lst != 0) {
        tok = car(car(macro_tok_lst));
        val = cdr(car(macro_tok_lst));
        macro_tok_lst = cdr(macro_tok_lst);
        // Tokens that are identifiers and up are tokens whose kind can change
        // between the moment the macro is defined and where it is used.
        // So we reload the kind from the ident table.
        if (tok >= IDENTIFIER) tok = heap[val + 2];

        // Check if the next token is ## for token pasting
        if (macro_tok_lst != 0 AND car(car(macro_tok_lst)) == HASH_HASH) {
          if (tok == MACRO OR tok == MACRO_ARG) {
            // If the token is a macro or macro arg, it must be expanded before pasting
            macro_tok_lst = cdr(macro_tok_lst); // We consume the ## token
            paste_last_token = true;
          } else {
            // macro_tok_lst is not empty because read_macro_tokens checked for trailing ##
            macro_tok_lst = cdr(macro_tok_lst); // Skip the ##
            paste_tokens(tok, val);
            break;
          }
        } else if (macro_tok_lst == 0 AND paste_last_token) {
          if (macro_stack_ix == 0) {
            // If we are not in a macro expansion, we can't paste the last token
            // This should not happen if the macro is well-formed, which is
            // checked by read_macro_tokens.
            fatal_error("## cannot appear at the end of a macro expansion");
          }
          macro_stack_ix -= 2;
          macro_tok_lst = macro_stack[macro_stack_ix];
          macro_args = macro_stack[macro_stack_ix + 1];
          paste_last_token = false; // We are done pasting
          paste_tokens(tok, val);
        }

        if (tok == MACRO) { // Nested macro expansion!
          if (attempt_macro_expansion(val)) {
            continue;
          }
          break;
        } else if (tok == MACRO_ARG AND expand_macro_arg) {
          push_macro(get_macro_arg(val), 0); // Play the tokens of the macro argument
          continue;
        } else if (tok == '#') { // Stringizing!
          stringify(tok, val);
          break;
        }
        break;
      } else if (macro_stack_ix != 0) {
        macro_stack_ix -= 2;
        macro_tok_lst = macro_stack[macro_stack_ix];
        macro_args = macro_stack[macro_stack_ix + 1];
        continue;
      } else if (ch <= ' ') {

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

        if (tok == MACRO) {
          // We only expand in ifdef true blocks and if the expander is enabled.
          // Since this is the "base case" of the macro expansion, we don't need
          // to disable the other places where macro expansion is done.
          if (ifdef_mask AND expand_macro) {
            if (attempt_macro_expansion(val)) {
              continue;
            }
            break;
          }
        }
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

        } else if (ch == '#') {

          get_ch();
          if (ch == '#') {
            get_ch();
            tok = HASH_HASH;
          }

          break;

        } else if ((ch == '~') OR (ch == '.') OR (ch == '?') OR (ch == ',') OR (ch == ':') OR (ch == ';') OR (ch == '(') OR (ch == ')') OR (ch == '[') OR (ch == ']') OR (ch == '{') OR (ch == '}')) {

          tok = ch;

          get_ch();

          break;

        } else if (ch == '\\') {
          get_ch();

          if (ch == '\n') { /* Continues with next token */
            get_ch();
          } else {
            putstr("ch="); putint(ch); putchar('\n');
            fatal_error("unexpected character after backslash");
          }
        } else {
          putstr("ch="); putint(ch); putchar('\n');
          fatal_error("invalid token");
        }
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

void expect_tok(int expected_tok) {
  if (tok != expected_tok) {
    putstr("expected_tok="); putint(expected_tok);
    putstr(" tok="); putint(tok); putchar('\n');
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
    if ((tok == INT_KW) OR (tok == SHORT_KW) OR (tok == LONG_KW) OR (tok == SIGNED_KW)) {
      if ((type_kw != 0) AND (type_kw != INT_KW)) {
        syntax_error("inconsistent type");
      } else {
        type_kw = INT_KW;
        get_tok();
      }
    } else if (tok == CHAR_KW) {
      if (type_kw != 0) {
        syntax_error("inconsistent type");
      } else {
        type_kw = CHAR_KW;
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
  ast tail;

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

    if (tok == STRING) { // Contiguous strings
      result = cons(result, 0);
      tail = result;
      while (tok == STRING) {
        set_cdr(tail, cons(new_ast0(STRING, val), 0));
        tail = cdr(tail);
        get_tok();
      }

      // Unpack the list of strings into a single string
      begin_string();

      while (result != 0) {
        accum_string_string(get_val(car(result)));
        result = cdr(result);
      }

      accum_string_char(0); // Null terminate the string

      result = new_ast0(STRING, string_start);
    }

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

    } else if (tok == PLUS_PLUS) {

      get_tok();
      result = new_ast1(PLUS_PLUS_POST, result);

    } else if (tok == MINUS_MINUS) {

      get_tok();
      result = new_ast1(MINUS_MINUS_POST, result);

    } else {
      break;
    }
  }

  return result;
}

ast parse_unary_expression() {

  ast result;
  int op;

  if (tok == PLUS_PLUS){

    get_tok();
    result = parse_unary_expression();
    result = new_ast1(PLUS_PLUS_PRE, result);

  } else if (tok == MINUS_MINUS) {

    get_tok();
    result = parse_unary_expression();
    result = new_ast1(MINUS_MINUS_PRE, result);

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

#ifndef DEBUG_CPP
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
#endif

//-----------------------------------------------------------------------------

#ifdef DEBUG_CPP
#include "debug.c"
#endif

int main(int argc, char **args) {

  int i = 1;

  init_ident_table();

  init_pnut_macros();

  while (i < argc) {
    if (args[i][0] == '-') {
      if (args[i][1] == 'D') {
        init_ident(MACRO, args[i] + 2);
      } else {
        putstr("Option ");
        putstr(args[i]);
        putchar('\n');
        fatal_error("unknown option");
      }
    } else {
      // Options that don't start with '-' are file names
      #ifdef SUPPORT_INCLUDE
      include_file(args[i]);
      #else
      fatal_error("input file not supported. Pnut expects the input from stdin.");
      #endif
    }
    i += 1;
  }

  #ifdef SUPPORT_INCLUDE
  if (fp == 0) {
    putstr("Usage: "); putstr(args[0]); putstr(" <filename>\n");
    fatal_error("no input file");
  }
  #endif

  #ifndef DEBUG_CPP
  codegen_begin();
  #endif

  ch = '\n';
  get_tok();

  while (tok != EOF) {
    #ifdef DEBUG_CPP
    print_tok(tok, val);
    get_tok();
    #else
    codegen_glo_decl(parse_definition(0));
    #endif
  }

  #ifndef DEBUG_CPP
  codegen_end();
  #endif

  return 0;
}
