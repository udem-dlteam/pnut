// Those includes are parsed by pnut but ignored
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <stdint.h> // for intptr_t

#define ast int
#define true 1
#define false 0
#define EOF (-1)

#ifdef RELEASE_PNUT_SH
#define sh
#define RT_NO_INIT_GLOBALS
#define RELEASE_PNUT
#endif

#ifdef RELEASE_PNUT_i386_linux
#define target_i386_linux
#define RELEASE_PNUT
#endif

#ifdef RELEASE_PNUT_x86_64_linux
#define target_x86_64_linux
#define RELEASE_PNUT
#endif

#ifdef RELEASE_PNUT_x86_64_mac
#define target_x86_64_mac
#define RELEASE_PNUT
#endif

#ifdef RELEASE_PNUT
#define INCLUDE_LINE_NUMBER_ON_ERROR
#define NICE_ERR_MSG
#define OPTIMIZE_LONG_LINES
#endif

// Uncomment to cause parse_error() to print which pnut function emitted the error
//#define DEBUG_SHOW_ERR_ORIGIN

// Use positional parameter directly for function parameters that are constants
#define OPTIMIZE_CONSTANT_PARAM_not
#define SUPPORT_ADDRESS_OF_OP_not

// Shell backend codegen options
#ifndef SH_AVOID_PRINTF_USE_NOT
#define SH_AVOID_PRINTF_USE
#endif
#define SH_INLINE_PUTCHAR
#define SH_INLINE_EXIT
// Specifies if we include the C code along with the generated shell code
#define SH_INCLUDE_C_CODE_not
// Have let commands initialize function parameters
#ifndef SH_SAVE_VARS_WITH_SET
#define SH_INITIALIZE_PARAMS_WITH_LET
#endif
// If we use the `set` command and positional parameters to simulate local vars
#if !defined(SH_SAVE_VARS_WITH_SET) && !defined(SH_INITIALIZE_PARAMS_WITH_LET)
#define SH_SAVE_VARS_WITH_SET
#endif
// Inline ascii code of character literal
#define SH_INLINE_CHAR_LITERAL_not

// Options to parameterize the shell runtime library
#ifndef RT_FREE_UNSETS_VARS_NOT
#define RT_FREE_UNSETS_VARS
#endif
#define RT_NO_INIT_GLOBALS_not
#define RT_COMPACT_not
#define RT_INLINE_PUTCHAR
#define RT_USE_LOOKUP_TABLE

// Make sure we don't use the long line optimization when RT_COMPACT is on
#ifdef RT_COMPACT
#undef OPTIMIZE_LONG_LINES
#endif

// Options that turns Pnut into a C preprocessor or some variant of it
// DEBUG_GETCHAR: Read and print the input character by character.
// DEBUG_CPP: Run preprocessor like gcc -E. This can be useful for debugging the preprocessor.
// DEBUG_EXPAND_INCLUDES: Reads the input file and includes the contents of the included files.
// DEBUG_PARSER: Runs the tokenizer on the input. Outputs nothing.

typedef int bool;

#ifdef PNUT_CC

typedef int FILE;

#endif

#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
int line_number = 1;
int column_number = 0;
int last_tok_line_number = 1;
int last_tok_column_number = 0;
#endif

struct IncludeStack {
  FILE* fp;
  struct IncludeStack *next;
  char *dirname;  // The base path of the file, used to resolve relative paths
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  char *filepath; // The path of the file, used to print error messages
  int line_number;
  int column_number;
#endif
};
struct IncludeStack *include_stack, *include_stack2;
FILE *fp = 0; // Current file pointer that's being read
char* fp_filepath = 0; // The path of the current file being read
char* include_search_path = 0; // Search path for include files

// Tokens and AST nodes
enum {
  AUTO_KW = 300,
  BREAK_KW,
  CASE_KW,
  CHAR_KW,
  CONST_KW,
  CONTINUE_KW,
  DEFAULT_KW,
  DO_KW,
  DOUBLE_KW,
  ELSE_KW,
  ENUM_KW,
  EXTERN_KW,
  FLOAT_KW,
  FOR_KW,
  GOTO_KW,
  IF_KW,
  INT_KW,
  LONG_KW,
  REGISTER_KW,
  RETURN_KW,
  SHORT_KW,
  SIGNED_KW,
  SIZEOF_KW,
  STATIC_KW,
  STRUCT_KW,
  SWITCH_KW,
  TYPEDEF_KW,
  UNION_KW,
  UNSIGNED_KW,
  VOID_KW,
  VOLATILE_KW,
  WHILE_KW,
  VAR_DECL,
  VAR_DECLS,
  FUN_DECL,
  CAST,

  // Non-character operands
  INTEGER    = 401,
  CHARACTER,
  STRING,
  AMP_AMP,
  AMP_EQ,
  ARROW,
  BAR_BAR,
  BAR_EQ,
  CARET_EQ,
  EQ_EQ,
  GT_EQ,
  LSHIFT_EQ,
  LSHIFT,
  LT_EQ,
  MINUS_EQ,
  MINUS_MINUS,
  EXCL_EQ,
  PERCENT_EQ,
  PLUS_EQ,
  PLUS_PLUS,
  RSHIFT_EQ,
  RSHIFT,
  SLASH_EQ,
  STAR_EQ,
  HASH_HASH,
  PLUS_PLUS_PRE,
  MINUS_MINUS_PRE,
  PLUS_PLUS_POST,
  MINUS_MINUS_POST,
  ELLIPSIS,
  PARENS,

  // Other tokens
  MACRO_ARG = 499,
  IDENTIFIER = 500,
  TYPE = 501,
  MACRO = 502,
};

void putstr(char *str) {
  while (*str) {
    putchar(*str);
    str += 1;
  }
}

void putint_aux(int n) {
  if (n <= -10) putint_aux(n / 10);
  putchar('0' - (n % 10));
}

void putint(int n) {
  if (n < 0) {
    putchar('-');
    putint_aux(n);
  } else {
    putint_aux(-n);
  }
}

void fatal_error(char *msg) {
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  if (include_stack != 0) {
    putstr(include_stack->filepath); putchar(':');
    putint(last_tok_line_number); putchar(':'); putint(last_tok_column_number);
    putstr("  "); putstr(msg); putchar('\n');
  } else {
    putstr(msg); putchar('\n');
  }
#else
  putstr(msg); putchar('\n');
#endif
  exit(1);
}

void syntax_error(char *msg) {
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  putstr(include_stack->filepath); putchar(':');
  putint(last_tok_line_number); putchar(':'); putint(last_tok_column_number);
  putstr("  syntax error: "); putstr(msg); putchar('\n');
#else
  putstr("syntax error: "); putstr(msg); putchar('\n');
#endif
  exit(1);
}

// tokenizer

int ch;
#ifdef DEBUG_EXPAND_INCLUDES
int prev_ch = EOF;
#endif
int tok;
int val;

#define STRING_POOL_SIZE 50000
char string_pool[STRING_POOL_SIZE];
int string_pool_alloc = 0;
int string_start;
int hash;

// These parameters give a perfect hashing of the C keywords
#define HASH_PARAM 1026
#define HASH_PRIME 1009
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

// Because everything is an int in pnut, it's easy to make mistakes and pass the
// wrong node type to a function. These versions of get_child take the input
// and/or output node type and checks that the node has the expected type before
// returning the child node.
#ifdef SAFE_MODE
// This function checks that the parent node has the expected operator before
// returning the child node.
ast get_child_go(char* file, int line, int expected_parent_node, ast node, int i) {
  if (get_op(node) != expected_parent_node) {
    printf("%s:%d: Expected node %d, got %d\n", file, line, expected_parent_node, get_op(node));
    exit(1);
  }
  return heap[node+i+1];
}

// This function checks that the parent node has the expected operator and that
// the child node has the expected operator before returning the child node.
ast get_child__go(char* file, int line, int expected_parent_node, int expected_node, ast node, int i) {
  if (get_op(node) != expected_parent_node) {
    printf("%s:%d: Expected node %d, got %d\n", file, line, expected_parent_node, get_op(node));
    exit(1);
  }
  if (get_op(heap[node+i+1]) != expected_node) {
    printf("%s:%d: Expected child node %d, got %d\n", file, line, expected_node, get_op(heap[node+i+1]));
    exit(1);
  }
  return heap[node+i+1];
}

// This function checks that the parent node has the expected operator and that
// the child node has the expected operator (if child node is not 0) before
// returning the child node.
ast get_child_opt_go(char* file, int line, int expected_parent_node, int expected_node, ast node, int i) {
  if (get_op(node) != expected_parent_node) {
    printf("%s:%d: Expected node %d, got %d\n", file, line, expected_parent_node, get_op(node));
    exit(1);
  }
  if (heap[node+i+1] > 0 && get_op(heap[node+i+1]) != expected_node) {
    printf("%s:%d: Expected child node %d, got %d\n", file, line, expected_node, get_op(heap[node+i+1]));
    exit(1);
  }
  return heap[node+i+1];
}
#define get_child_(expected_parent_node, node, i) get_child_go(__FILE__, __LINE__, expected_parent_node, node, i)
#define get_child__(expected_parent_node, expected_node, node, i) get_child__go(__FILE__, __LINE__, expected_parent_node, expected_node, node, i)
#define get_child_opt_(expected_parent_node, expected_node, node, i) get_child_opt_go(__FILE__, __LINE__, expected_parent_node, expected_node, node, i)
#else
#define get_child_(expected_parent_node, node, i) get_child(node, i)
#define get_child__(expected_parent_node, expected_node, node, i) get_child(node, i)
#define get_child_opt_(expected_parent_node, expected_node, node, i) get_child(node, i)
#endif

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

ast clone_ast(ast orig) {
  int nb_children = get_nb_children(orig);
  int i;

  // Account for the value of ast nodes with no child
  if (nb_children == 0) nb_children = 1;

  ast_result = alloc_obj(nb_children + 1);

  heap[ast_result] = heap[orig]; // copy operator and nb of children
  for (i = 0; i < nb_children; i += 1) {
    set_child(ast_result, i, get_child(orig, i));
  }

  return ast_result;
}

// Simple accessor to get the string from the string pool
#define STRING_BUF(string_val) (string_pool + heap[string_val+1])
#define STRING_LEN(string_val) (heap[string_val+4])

void begin_string() {
  string_start = string_pool_alloc;
  hash = 0;
}

// Append the current character (ch) to the string under construction in the pool
void accum_string() {
  hash = (ch + (hash ^ HASH_PARAM)) % HASH_PRIME;
  string_pool[string_pool_alloc] = ch;
  string_pool_alloc += 1;
  if (string_pool_alloc >= STRING_POOL_SIZE) {
    fatal_error("string pool overflow");
  }
}

// Append a character to the current string under construction in the pool
void accum_string_char(char c) {
  hash = (c + (hash ^ HASH_PARAM)) % HASH_PRIME;
  string_pool[string_pool_alloc] = c;
  string_pool_alloc += 1;
  if (string_pool_alloc >= STRING_POOL_SIZE) {
    fatal_error("string pool overflow");
  }
}

// Append a string from the string_pool to the string under construction
void accum_string_string(int string_probe) {
  char *string_start = STRING_BUF(string_probe);
  char *string_end = string_start + STRING_LEN(string_probe);
  while (string_start < string_end) {
    accum_string_char(*string_start);
    string_start += 1;
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

// Like end_ident, but for strings instead of identifiers
// We want to deduplicate strings to reuse memory if possible.
#define end_string end_ident

int end_ident() {
  string_pool[string_pool_alloc] = 0; // terminate string
  string_pool_alloc += 1; // account for terminator

  probe = heap[hash];

  while (probe != 0) {
    probe_start = heap[probe+1];
    end_ident_i = 0;
    c1 = string_pool[string_start+end_ident_i];
    c2 = string_pool[probe_start+end_ident_i];
    while (c1 == c2) {
      if (c1 == 0) {
        string_pool_alloc = string_start; // undo string allocation
        return probe;
      }
      end_ident_i += 1;
      c1 = string_pool[string_start+end_ident_i];
      c2 = string_pool[probe_start+end_ident_i];
    }
    hash = probe; // remember previous ident
    probe = heap[probe];
  }

  // a new ident has been found

  probe = alloc_obj(5);

  heap[hash] = probe; // add new ident at end of chain

  heap[probe] = 0; // no next ident
  heap[probe+1] = string_start;
  heap[probe+2] = IDENTIFIER;
  heap[probe+3] = 0; // Token tag
  heap[probe+4] = string_pool_alloc - string_start - 1; // string length

  return probe;
}

void get_tok();
void get_ident();
void expect_tok(int expected);

#define IFDEF_DEPTH_MAX 20
bool if_macro_stack[IFDEF_DEPTH_MAX]; // Stack of if macro states
bool if_macro_stack_ix = 0;
bool if_macro_mask = true;      // Indicates if the current if/elif block is being executed
bool if_macro_executed = false; // If any of the previous if/elif conditions were true
int  if_macro_nest_level = 0;      // Current number of unmatched #if/#ifdef/#ifndef directives that were masked out

// get_tok parameters:
// Whether to expand macros or not.
// Useful to parse macro definitions containing other macros without expanding them.
bool expand_macro = true;
// Don't expand macro arguments. Used for stringification and token pasting.
bool expand_macro_arg = true;
// Don't produce newline tokens. Used when reading the tokens of a macro definition.
bool skip_newlines = true;

#define MACRO_RECURSION_MAX 100
int macro_stack[MACRO_RECURSION_MAX];
int macro_stack_ix = 0;

int macro_tok_lst = 0;  // Current list of tokens to replay for the macro being expanded
int macro_args = 0;     // Current list of arguments for the macro being expanded
int macro_args_count;   // Number of arguments for the current macro being expanded
bool paste_last_token = false; // Whether the last token was a ## or not

void push_if_macro_mask(bool new_mask) {
  if (if_macro_stack_ix >= IFDEF_DEPTH_MAX) {
    fatal_error("Too many nested #ifdef/#ifndef directives. Maximum supported is 20.");
  }
  // Save current mask on the stack because it's about to be overwritten
  if_macro_stack[if_macro_stack_ix] = if_macro_mask;
  if_macro_stack[if_macro_stack_ix + 1] = if_macro_executed;
  if_macro_stack_ix += 2;
  // Then set the new mask value and reset the executed flag
  if_macro_mask = if_macro_executed = new_mask;
}

void pop_if_macro_mask() {
  if (if_macro_stack_ix == 0) {
    fatal_error("Unbalanced #ifdef/#ifndef/#else/#endif directives.");
  }
  if_macro_stack_ix -= 2;
  if_macro_mask = if_macro_stack[if_macro_stack_ix];
  if_macro_executed = if_macro_stack[if_macro_stack_ix + 1];
}

// Includes the preprocessed C code along with the generated shell code
#ifdef SH_INCLUDE_C_CODE
#define DECLARATION_BUF_LEN 20000

char declaration_char_buf[DECLARATION_BUF_LEN];
int declaration_char_buf_ix = 0;
// Point to the last character of the last token.
// This is used to skip the current token when printing the code of a
// declaration since it belongs to the next declaration.
int last_tok_char_buf_ix = 0;

void output_declaration_c_code(bool no_header) {

  int i = 0;

  if (!no_header) {
    putstr("#################################### C code ####################################\n");
  }
  putchar('#');
  putchar(' ');

  // Skip leading newlines if any.
  while (declaration_char_buf[i] == '\n') i += 1;

  for (; i < last_tok_char_buf_ix; i += 1) {

    if (declaration_char_buf[i] == '\n') {
      // Condense the C code by removing extra newlines
      if (declaration_char_buf[i - 1] != declaration_char_buf[i]) {
        putchar('\n');
        putchar('#');
        putchar(' ');
      }
    } else {
      putchar(declaration_char_buf[i]);
    }
  }

  // End of decl
  putchar('\n');
  if (!no_header) {
    putstr("################################# End of C code ################################\n");
  }

  // Copy the last token characters to the beginning of the buffer
  for (i = 0; i < declaration_char_buf_ix - last_tok_char_buf_ix; i += 1) {
    declaration_char_buf[i] = declaration_char_buf[last_tok_char_buf_ix + i];
  }

  declaration_char_buf_ix = i;
}
#endif

void get_ch() {
  ch = fgetc(fp);
  if (ch == EOF) {
    // If it's not the last file on the stack, EOF means that we need to switch to the next file
    if (include_stack->next != 0) {
      fclose(include_stack->fp);
      include_stack2 = include_stack;
      include_stack = include_stack->next;
      fp = include_stack->fp;
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
      line_number = include_stack->line_number;
      column_number = include_stack->column_number;
#endif
      // Not freeing include_stack2->filepath because it may not be dynamically allocated
      free(include_stack2->dirname);
      free(include_stack2);
      // EOF is treated as a newline so that files without a newline at the end are still parsed correctly
      // On the next get_ch call, the first character of the next file will be read
      ch = '\n';
    }
  }
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  else if (ch == '\n') {
    line_number += 1;
    column_number = 0;
  } else {
    column_number += 1;
  }
#endif
#ifdef SH_INCLUDE_C_CODE
  // Save C code chars so they can be displayed with the shell code
  declaration_char_buf[declaration_char_buf_ix] = ch;
  declaration_char_buf_ix += 1;
#endif
#ifdef DEBUG_EXPAND_INCLUDES
  // Because ch is always 1 character ahead of the token, we print the character
  // with a 1 character delay to match this delay. This makes it easy to
  // annotate certain preprocessor directives so they can be removed in a later
  // step.
  if (prev_ch != EOF) putchar(prev_ch);
  prev_ch = ch;
#endif
}

#ifdef PNUT_CC
// TODO: It would be nice to not have to duplicate this code
int strlen(char *str) {
  int i = 0;
  while (str[i] != '\0') i += 1;
  return i;
}

void memcpy(char *dest, char *src, int n) {
  int i;
  for (i = 0; i < n; i += 1) {
    dest[i] = src[i];
  }
}

#endif

char *substr(char *str, int start, int end) {
  int len = end - start;
  char *temp = malloc(len + 1);
  memcpy(temp, str + start, len);
  temp[len] = '\0';
  return temp;
}

char *str_concat(char *s1, char *s2) {
  int s1_len = strlen(s1);
  int s2_len = strlen(s2);
  char *temp = malloc(s1_len + s2_len + 1);
  memcpy(temp, s1, s1_len);
  memcpy(temp + s1_len, s2, s2_len);
  temp[s1_len + s2_len] = '\0';
  return temp;
}

// Removes the last component of the path, keeping the trailing slash if any.
// For example, /a/b/c.txt -> /a/b/
// If the path does not contain a slash, it returns "".
char *file_parent_directory(char *path) {
  int i = 0;
  int last_slash = -1;
  while (path[i] != '\0') {
    if (path[i] == '/') last_slash = i;

    i += 1;
  }
  if (last_slash == -1) {
    path = malloc(1);
    path[0] = '\0';
  } else {
    path = substr(path, 0, last_slash + 1);
  }
  return path;
}

FILE *fopen_source_file(char *file_name, char *relative_to) {
  FILE *fp;
  fp_filepath = file_name;
  if (relative_to) {
    fp_filepath = str_concat(relative_to, fp_filepath);
  }
  fp = fopen(fp_filepath, "r");
  if (fp == 0) {
    putstr("Could not open file: "); putstr(fp_filepath); putchar('\n');
    exit(1);
  }
  return fp;
}

void include_file(char *file_name, char *relative_to) {
  fp = fopen_source_file(file_name, relative_to);
  include_stack2 = malloc(sizeof(struct IncludeStack));
  include_stack2->next = include_stack;
  include_stack2->fp = fp;
  include_stack2->dirname = file_parent_directory(fp_filepath);
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  include_stack2->filepath = fp_filepath;
  include_stack2->line_number = 1;
  include_stack2->column_number = 0;
  // Save the current file position so we can return to it after the included file is done
  if (include_stack != 0) {
    include_stack->line_number = line_number;
    include_stack->column_number = column_number;
  }
  line_number = 1;
  column_number = 1;
#endif
  include_stack = include_stack2;
}

int accum_digit(int base) {
  int digit = 99;
  if ('0' <= ch && ch <= '9') {
    digit = ch - '0';
  } else if ('A' <= ch && ch <= 'Z') {
    digit = ch - 'A' + 10;
  } else if ('a' <= ch && ch <= 'z') {
    digit = ch - 'a' + 10;
  }
  if (digit >= base) {
    return 0; // character is not a digit in that base
  } else {
    // TODO: Put overflow check back
    // if ((val < limit) || ((val == limit) && (digit > limit * base - MININT))) {
    //   fatal_error("literal integer overflow");
    // }

    val = val * base - digit;
    get_ch();
    return 1;
  }
}

void get_string_char() {

  val = ch;
  get_ch();

  if (val == '\\') {
    if ('0' <= ch && ch <= '7') {
      // Parse octal character, up to 3 digits.
      // Note that \1111 is parsed as '\111' followed by '1'
      // See https://en.wikipedia.org/wiki/Escape_sequences_in_C#Notes
      val = 0;
      accum_digit(8);
      accum_digit(8);
      accum_digit(8);
      val = -(val % 256); // keep low 8 bits, without overflowing
    } else if (ch == 'x' || ch == 'X') {
      get_ch();
      val = 0;
      // Allow 1 or 2 hex digits.
      if (accum_digit(16)) {
        accum_digit(16);
      } else {
        syntax_error("invalid hex escape -- it must have at least one digit");
      }
      val = -(val % 256); // keep low 8 bits, without overflowing
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
      } else if (ch == '\\' || ch == '\'' || ch == '\"') {
        val = ch;
      } else {
        syntax_error("unimplemented string character escape");
      }
      get_ch();
    }
  }
}

void accum_string_until(char end) {
  while (ch != end && ch != EOF) {
    get_string_char();
    tok = ch;
    ch = val;
    accum_string();
    ch = tok;
  }

  if (ch != end) {
    syntax_error("unterminated string literal");
  }

  get_ch();
}

// We add the preprocessor keywords to the ident table so they can be easily
// recognized by the preprocessor. Because these are not C keywords, their kind
// is still IDENTIFIER so the parser (which runs after the preprocessor) can
// treat them as such.
int IFDEF_ID;
int IFNDEF_ID;
int ELIF_ID;
int ENDIF_ID;
int DEFINE_ID;
int UNDEF_ID;
int INCLUDE_ID;
int DEFINED_ID;
int WARNING_ID;
int ERROR_ID;
int INCLUDE_SHELL_ID;

int NOT_SUPPORTED_ID;

// We want to recognize certain identifers without having to do expensive string comparisons
int ARGV__ID;
int ARGV_ID;
int IFS_ID;
int MAIN_ID;

int PUTCHAR_ID;
int GETCHAR_ID;
int EXIT_ID;
int MALLOC_ID;
int FREE_ID;
int PRINTF_ID;
int FOPEN_ID;
int FCLOSE_ID;
int FGETC_ID;
int PUTSTR_ID;
int PUTS_ID;
int READ_ID;
int WRITE_ID;
int OPEN_ID;
int CLOSE_ID;

// Macros that are defined by the preprocessor
int FILE__ID;
int LINE__ID;
int DATE__ID;
int TIME__ID;
int TIMESTAMP__ID;

// When we parse a macro, we generally want the tokens as they are, without expanding them.
void get_tok_macro() {
  bool prev_expand_macro = expand_macro;
  bool prev_macro_mask = if_macro_mask;
  bool skip_newlines_prev = skip_newlines;

  expand_macro = false;
  if_macro_mask = true;
  skip_newlines = false;
  get_tok();
  expand_macro = prev_expand_macro;
  if_macro_mask = prev_macro_mask;
  skip_newlines = skip_newlines_prev;
}

// Like get_tok_macro, but skips newline
// This is useful when we want to read the arguments of a macro expansion.
void get_tok_macro_expand() {
  bool prev_expand_macro = expand_macro;
  bool prev_macro_mask = if_macro_mask;

  expand_macro = false;
  if_macro_mask = true;
  get_tok();
  expand_macro = prev_expand_macro;
  if_macro_mask = prev_macro_mask;
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
  if (tok != '\n' && tok != EOF) {
    // Append the token/value pair to the replay list
    toks = cons(lookup_macro_token(args, tok, val), 0);
    tail = toks;
    get_tok_macro();
    while (tok != '\n' && tok != EOF) {
      heap[tail + 1] = cons(lookup_macro_token(args, tok, val), 0);
      tail = cdr(tail); // Advance tail
      get_tok_macro();
    }

    // Check that there are no leading or trailing ##
    if (car(car(toks)) == HASH_HASH || car(car(tail)) == HASH_HASH) {
      syntax_error("'##' cannot appear at either end of a macro expansion");
    }
  }

  return toks;
}

// A few things that are different from the standard:
// - We allow sequence of commas in the argument list
// - Function-like macros with 0 arguments can be called either without parenthesis or with ().
// - No support for variadic macros. Tcc only uses them in tests so it should be ok.
void handle_define() {
  int macro;    // The identifier that is being defined as a macro
  int args = 0; // List of arguments for a function-like macro
  int args_count = -1; // Number of arguments for a function-like macro. -1 means it's an object-like macro

  if (tok != IDENTIFIER && tok != MACRO && (tok < AUTO_KW || tok > WHILE_KW)) {
    putstr("tok="); putint(tok); putchar('\n');
    syntax_error("#define directive can only be followed by a identifier");
  }

  heap[val + 2] = MACRO; // Mark the identifier as a macro
  macro = val;
  if (ch == '(') { // Function-like macro
    args_count = 0;
    get_tok_macro(); // Skip macro name
    get_tok_macro(); // Skip '('
    while (tok != '\n' && tok != EOF) {
      if (tok == ',') {
        // Allow sequence of commas, this is more lenient than the standard
        get_tok_macro();
        continue;
      } else if (tok == ')') {
        get_tok_macro();
        break;
      }
      get_tok_macro();
      // Accumulate parameters in reverse order. That's ok because the arguments
      // to the macro will also be in reverse order.
      args = cons(val, args);
      args_count += 1;
    }
  } else {
    get_tok_macro(); // Skip macro name
  }

  // Accumulate tokens so they can be replayed when the macro is used
  heap[macro + 3] = cons(read_macro_tokens(args), args_count);

}

int eval_constant(ast expr, bool if_macro) {
  int op = get_op(expr);
  int op1;
  int op2;
  ast child0, child1;

  if (get_nb_children(expr) >= 1) child0 = get_child(expr, 0);
  if (get_nb_children(expr) >= 2) child1 = get_child(expr, 1);

  switch (op) {
    case PARENS:    return eval_constant(child0, if_macro);
    case INTEGER:   return -get_val(expr);
    case CHARACTER: return get_val(expr);
    case '~':       return ~eval_constant(child0, if_macro);
    case '!':       return !eval_constant(child0, if_macro);
    case '-':
    case '+':
      op1 = eval_constant(child0, if_macro);
      if (get_nb_children(expr) == 1) {
        return op == '-' ? -op1 : op1;
      } else {
        op2 = eval_constant(child1, if_macro);
        return op == '-' ? op1 - op2 : op1 + op2;
      }

    case '?':
      op1 = eval_constant(child0, if_macro);
      if (op1) {
        return eval_constant(child1, if_macro);
      } else {
        return eval_constant(get_child(expr, 2), if_macro);
      }

    case '*':
    case '/':
    case '%':
    case '&':
    case '|':
    case '^':
    case LSHIFT:
    case RSHIFT:
    case EQ_EQ:
    case EXCL_EQ:
    case LT_EQ:
    case GT_EQ:
    case '<':
    case '>':
      op1 = eval_constant(child0, if_macro);
      op2 = eval_constant(child1, if_macro);
      switch (op) {
        case '*':     return op1 * op2;
        case '/':     return op1 / op2;
        case '%':     return op1 % op2;
        case '&':     return op1 & op2;
        case '|':     return op1 | op2;
        case '^':     return op1 ^ op2;
        case LSHIFT:  return op1 << op2;
        case RSHIFT:  return op1 >> op2;
        case EQ_EQ:   return op1 == op2;
        case EXCL_EQ: return op1 != op2;
        case LT_EQ:   return op1 <= op2;
        case GT_EQ:   return op1 >= op2;
        case '<':     return op1 < op2;
        case '>':     return op1 > op2;
      }
      return 0; // Should never reach here

    case AMP_AMP:
      op1 = eval_constant(child0, if_macro);
      if (!op1) return 0;
      else return eval_constant(child1, if_macro);

    case BAR_BAR:
      op1 = eval_constant(child0, if_macro);
      if (op1) return 1;
      else return eval_constant(child1, if_macro);

    case '(': // defined operators are represented as fun calls
      if (if_macro && get_val(child0) == DEFINED_ID) {
        return child1 == MACRO;
      } else {
        fatal_error("unknown function call in constant expressions");
        return 0;
      }

    case IDENTIFIER:
      if (if_macro) {
        // Undefined identifiers are 0
        // At this point, macros have already been expanded so we can't have a macro identifier
        return 0;
      } else {
        // TODO: Enums when outside of if_macro
        fatal_error("identifiers are not allowed in constant expression");
        return 0;
      }

    default:
      putstr("op="); putint(op); putchar('\n');
      fatal_error("unsupported operator in constant expression");
      return 0;
  }
}

ast parse_assignment_expression();

int evaluate_if_condition() {
  bool prev_skip_newlines = skip_newlines;
  int previous_mask = if_macro_mask;
  ast expr;
  // Temporarily set to true so that we can read the condition even if it's inside an ifdef false block
  // Unlike in other directives using get_tok_macro, we want to expand macros in the condition
  if_macro_mask = true;
  skip_newlines = false; // We want to stop when we reach the first newline
  get_tok(); // Skip the #if keyword
  expr = parse_assignment_expression();

  // Restore the previous value
  if_macro_mask = previous_mask;
  skip_newlines = prev_skip_newlines;
  return eval_constant(expr, true);
}

void handle_include() {
  if (tok == STRING) {
    include_file(STRING_BUF(val), include_stack->dirname);
#ifdef DEBUG_EXPAND_INCLUDES
    // When running pnut in "expand includes" mode, we want to annotate the
    // #include directives that were expanded with a comment so we can remove
    // them later.
    putstr(" // INCLUDED");
#endif
    get_tok_macro(); // Skip the string
  } else if (tok == '<') {
    accum_string_until('>');
    val = end_string();
    // #include <file> directives only take effect if the search path is provided
    // TODO: Issue a warning to stderr when skipping the directive
    if (include_search_path != 0) {
      include_file(STRING_BUF(val), include_search_path);
    }
    get_tok_macro(); // Skip the string
  } else {
    putstr("tok="); putint(tok); putchar('\n');
    syntax_error("expected string to #include directive");
  }
}

#ifdef sh
void handle_shell_include();
#endif

void handle_preprocessor_directive() {
  int temp;
#ifdef SH_INCLUDE_C_CODE
  int prev_char_buf_ix = declaration_char_buf_ix;
#endif
  get_tok_macro(); // Get the # token
  get_tok_macro(); // Get the directive

  if (tok == IDENTIFIER && (val == IFDEF_ID || val == IFNDEF_ID)) {
    temp = val;
    get_tok_macro(); // Get the macro name
    if (if_macro_mask) {
      push_if_macro_mask(temp == IFDEF_ID ? tok == MACRO : tok != MACRO);
    } else {
      // Keep track of the number of #ifdef so we can skip the corresponding #endif
      if_macro_nest_level += 1;
    }
    get_tok_macro(); // Skip the macro name
  } else if (tok == IF_KW) {
    temp = evaluate_if_condition();
    if (if_macro_mask) {
      push_if_macro_mask(temp);
    } else {
      // Keep track of the number of #ifdef so we can skip the corresponding #endif
      if_macro_nest_level += 1;
    }
  } else if (tok == IDENTIFIER && val == ELIF_ID) {
    temp = evaluate_if_condition();
    if (if_macro_executed) {
      // The condition is true, but its ignored if one of the conditions before was also true
      if_macro_mask = false;
    } else {
      if_macro_executed |= temp;
      if_macro_mask = temp;
    }
  } else if (tok == ELSE_KW) {
    if (if_macro_mask || if_macro_nest_level == 0) {
      if_macro_mask = !if_macro_executed;
    }
    get_tok_macro(); // Skip the else keyword
  } else if (tok == IDENTIFIER && val == ENDIF_ID) {
    if (if_macro_mask || if_macro_nest_level == 0) {
      pop_if_macro_mask();
    } else {
      if_macro_nest_level -= 1;
    }
    get_tok_macro(); // Skip the else keyword
  } else if (if_macro_mask) {
    if (tok == IDENTIFIER && val == INCLUDE_ID) {
      get_tok_macro(); // Get the STRING token
      handle_include();
    }
#ifdef sh
    // Not standard C, but serves to mix existing shell code with compiled C code
    else if (tok == IDENTIFIER && val == INCLUDE_SHELL_ID) {
      get_tok_macro(); // Get the STRING token
      handle_shell_include();
    }
#endif
    else if (tok == IDENTIFIER && val == UNDEF_ID) {
      get_tok_macro(); // Get the macro name
      if (tok == IDENTIFIER || tok == MACRO) {
        // TODO: Doesn't play nice with typedefs, because they are not marked as macros
        heap[val + 2] = IDENTIFIER; // Unmark the macro identifier
        get_tok_macro(); // Skip the macro name
      } else {
        putstr("tok="); putint(tok); putchar('\n');
        syntax_error("#undef directive can only be followed by a identifier");
      }
    } else if (tok == IDENTIFIER && val == DEFINE_ID) {
      get_tok_macro(); // Get the macro name
      handle_define();
    } else if (tok == IDENTIFIER && (val == WARNING_ID || val == ERROR_ID)) {
#ifndef DEBUG_EXPAND_INCLUDES
      temp = val;
      putstr(temp == WARNING_ID ? "warning:" : "error:");
      // Print the rest of the line, it does not support \ at the end of the line but that's ok
      while (ch != '\n' && ch != EOF) {
        putchar(ch); get_ch();
      }
      putchar('\n');
      tok = '\n';
      if (temp == ERROR_ID) exit(1);
#else
      tok = '\n';
#endif
    } else {
      putstr("tok="); putint(tok); putstr(": "); putstr(STRING_BUF(val)); putchar('\n');
      syntax_error("unsupported preprocessor directive");
    }
  } else {
    // Skip the rest of the directive
    while (tok != '\n' && tok != EOF) get_tok_macro();
  }

  if (tok != '\n' && tok != EOF) {
    putstr("tok="); putint(tok); putchar('\n');
    putstr("directive="); putint(tok); putchar('\n');
    putstr("string="); putstr(STRING_BUF(val)); putchar('\n');
    if (tok == IDENTIFIER || tok == MACRO) {
      putstr("string = ");
      putstr(STRING_BUF(val));
      putchar('\n');
    }
    syntax_error("preprocessor expected end of line");
  }

  // Because handle_preprocessor_directive is called from get_tok, and it loops
  // after the call to handle_preprocessor_directive, we don't need to call
  // get_tok before returning.

#ifdef SH_INCLUDE_C_CODE
  declaration_char_buf_ix = prev_char_buf_ix - 1; // - 1 to undo the #
#endif
}

void get_ident() {

  begin_string();

  while (('A' <= ch && ch <= 'Z') ||
         ('a' <= ch && ch <= 'z') ||
         ('0' <= ch && ch <= '9') ||
         (ch == '_')) {
    accum_string();
    get_ch();
  }

  val = end_ident();
  tok = heap[val+2];
}

int intern_str(char* name) {
  int i = 0;
  int prev_ch = ch; // The character may be important to the calling function, saving it

  begin_string();

  while (name[i] != 0) {
    ch = name[i];
    accum_string();
    i += 1;
  }

  i = end_string();

  ch = prev_ch;

  return i;
}

int init_ident(int tok, char *name) {
  int i = intern_str(name);
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
  init_ident(DO_KW,       "do");
  init_ident(DOUBLE_KW,   "double");
  init_ident(ELSE_KW,     "else");
  init_ident(ENUM_KW,     "enum");
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
  ELIF_ID    = init_ident(IDENTIFIER, "elif");
  ENDIF_ID   = init_ident(IDENTIFIER, "endif");
  DEFINE_ID  = init_ident(IDENTIFIER, "define");
  WARNING_ID = init_ident(IDENTIFIER, "warning");
  ERROR_ID   = init_ident(IDENTIFIER, "error");
  UNDEF_ID   = init_ident(IDENTIFIER, "undef");
  INCLUDE_ID = init_ident(IDENTIFIER, "include");
  DEFINED_ID = init_ident(IDENTIFIER, "defined");
  INCLUDE_SHELL_ID = init_ident(IDENTIFIER, "include_shell");

  ARGV_ID = init_ident(IDENTIFIER, "argv");
  ARGV__ID = init_ident(IDENTIFIER, "argv_");
  IFS_ID  = init_ident(IDENTIFIER, "IFS");
  MAIN_ID = init_ident(IDENTIFIER, "main");

  PUTCHAR_ID = init_ident(IDENTIFIER, "putchar");
  GETCHAR_ID = init_ident(IDENTIFIER, "getchar");
  EXIT_ID    = init_ident(IDENTIFIER, "exit");
  MALLOC_ID  = init_ident(IDENTIFIER, "malloc");
  FREE_ID    = init_ident(IDENTIFIER, "free");
  PRINTF_ID  = init_ident(IDENTIFIER, "printf");
  FOPEN_ID   = init_ident(IDENTIFIER, "fopen");
  FCLOSE_ID  = init_ident(IDENTIFIER, "fclose");
  FGETC_ID   = init_ident(IDENTIFIER, "fgetc");
  PUTSTR_ID  = init_ident(IDENTIFIER, "putstr");
  PUTS_ID    = init_ident(IDENTIFIER, "puts");
  READ_ID    = init_ident(IDENTIFIER, "read");
  WRITE_ID   = init_ident(IDENTIFIER, "write");
  OPEN_ID    = init_ident(IDENTIFIER, "open");
  CLOSE_ID   = init_ident(IDENTIFIER, "close");

  // Stringizing is recognized by the macro expander, but it returns a hardcoded
  // string instead of the actual value. This may be enough to compile TCC.
  NOT_SUPPORTED_ID = init_ident(IDENTIFIER, "NOT_SUPPORTED");
}

void init_builtin_string_macro(int macro_id, char* value) {
  // Macro object shape: ([(tok, val)], arity). -1 arity means it's an object-like macro
  heap[macro_id + 3] = cons(cons(cons(STRING, intern_str(value)), 0), -1);
}

void init_builtin_int_macro(int macro_id, int value) {
  heap[macro_id + 3] = cons(cons(cons(INTEGER, -value), 0), -1);
}

void init_pnut_macros() {
  init_ident(MACRO, "PNUT_CC");
  FILE__ID      = init_ident(MACRO, "__FILE__");
  LINE__ID      = init_ident(MACRO, "__LINE__");
  DATE__ID      = init_ident(MACRO, "__DATE__");
  TIME__ID      = init_ident(MACRO, "__TIME__");
  TIMESTAMP__ID = init_ident(MACRO, "__TIMESTAMP__");

  init_builtin_string_macro(FILE__ID, "<unknown>");
  init_builtin_int_macro   (LINE__ID, 0);
  init_builtin_string_macro(DATE__ID, "Jan  1 1970");
  init_builtin_string_macro(TIME__ID, "00:00:00");
  init_builtin_string_macro(TIMESTAMP__ID, "Jan  1 1970 00:00:00");
}

// A macro argument is represented using a list of tokens.
// Macro arguments are split by commas, but commas can also appear in function
// calls and as operators. To distinguish between the two, we need to keep track
// of the parenthesis depth.
int macro_parse_argument() {
  int arg_tokens = 0;
  int parens_depth = 0;
  int tail;

  while ((parens_depth > 0 || (tok != ',' && tok != ')')) && tok != EOF) {
    if (tok == '(') parens_depth += 1; // Enter parenthesis
    if (tok == ')') parens_depth -= 1; // End of parenthesis

    if (arg_tokens == 0) {
      arg_tokens = cons(cons(tok, val), 0);
      tail = arg_tokens;
    } else {
      heap[tail + 1] = cons(cons(tok, val), 0);
      tail = cdr(tail);
    }
    get_tok_macro_expand();
  }

  return arg_tokens;
}

void check_macro_arity(int macro_args_count, int macro) {
  int expected_argc = cdr(heap[macro + 3]);
  if (macro_args_count != expected_argc) {
    putstr("expected_argc="); putint(expected_argc);
    putstr(" != macro_args_count="); putint(macro_args_count);
    putchar('\n');
    putstr("macro="); putstr(STRING_BUF(macro)); putchar('\n');
    syntax_error("macro argument count mismatch");
  }
}

// Reads the arguments of a macro call, where the arguments are split by commas.
// Note that args are accumulated in reverse order, as the macro arguments refer
// to the tokens in reverse order.
int get_macro_args_toks(int macro) {
  int args = 0;
  int macro_args_count = 0;
  bool prev_is_comma = false;
  get_tok_macro_expand(); // Skip the macro identifier

  if (tok != '(') { // Function-like macro with 0 arguments
    check_macro_arity(macro_args_count, macro);
    return -1; // No arguments
  }

  get_tok_macro_expand(); // Skip '('

  while (tok != ')' && tok != EOF) {
    if (tok == ',') {
      get_tok_macro_expand(); // Skip comma
      if (prev_is_comma) { // Push empty arg
        args = cons(0, args);
        macro_args_count += 1;
      }
      prev_is_comma = true;
      continue;
    } else {
      prev_is_comma = false;
    }

    args = cons(macro_parse_argument(), args);
    macro_args_count += 1;
  }

  if (tok != ')') syntax_error("unterminated macro argument list");

  if (prev_is_comma) {
    args = cons(0, args); // Push empty arg
    macro_args_count += 1;
  }

  check_macro_arity(macro_args_count, macro);

  return args;
}

int get_macro_arg(int ix) {
  int arg = macro_args;
  while (ix > 0) {
    if (arg == 0) syntax_error("too few arguments to macro");
    arg = cdr(arg);
    ix -= 1;
  }
  return car(arg);
}

void play_macro(int tokens, int args) {
  if (tokens != 0) {
    if (macro_tok_lst != 0) {
      if (macro_stack_ix + 2 >= MACRO_RECURSION_MAX) {
        syntax_error("Macro recursion depth exceeded.");
      }
      macro_stack[macro_stack_ix] = macro_tok_lst;
      macro_stack[macro_stack_ix + 1] = macro_args;
      macro_stack_ix += 2;
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
  // We must save the tokens because the macro may be redefined while reading the arguments
  int tokens = car(heap[macro + 3]);
  macro = val;
  if (cdr(heap[macro + 3]) == -1) { // Object-like macro
    // Note: Redefining __{FILE,LINE}__ macros, either with the #define or #line
    // directives is not supported.
    if (macro == FILE__ID) {
      play_macro(cons(cons(STRING, intern_str(fp_filepath)), 0), 0);
    }
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
    else if (macro == LINE__ID) {
      play_macro(cons(cons(INTEGER, -line_number), 0), 0);
    }
#endif
    else {
      play_macro(tokens, 0);
    }
    return true;
  } else {
    new_macro_args = get_macro_args_toks(macro);
    // There was no argument list, i.e. not a function-like macro call even though it is a function-like macro
    if (new_macro_args == -1) {
      // get_macro_args_toks looked at the next token so we need to save it
      play_macro(cons(cons(tok, val), 0), 0);
      tok = IDENTIFIER;
      val = macro;
      return false;
    } else {
      play_macro(tokens, new_macro_args);
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
    syntax_error("expected macro argument after #");
  }
  arg = get_macro_arg(val);
  tok = STRING;
  // Support the case where the argument is a single identifier token
  if (car(car(arg)) == IDENTIFIER && cdr(arg) == 0) {
    val = cdr(car(arg)); // Use the identifier probe
  } else {
    val = NOT_SUPPORTED_ID; // Return string "NOT_SUPPORTED"
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
  expand_macro_arg = false;
  get_tok_macro();
  expand_macro_arg = true;
  // We need to handle the case where the right-hand side is a macro argument that expands to empty
  // In that case, the left-hand side is returned as is.
  if (tok == MACRO_ARG) {
    if (get_macro_arg(val) == 0) {
      tok = left_tok;
      val = left_val;
      return;
    } else {
      play_macro(get_macro_arg(val), 0); // Play the tokens of the macro argument
      get_tok_macro();
    }
  }
  right_tok = tok;
  right_val = val;
  if (left_tok == IDENTIFIER || left_tok == MACRO || left_tok <= WHILE_KW) {
    // Something that starts with an identifier can only be an identifier
    begin_string();
    accum_string_string(left_val);

    if (right_tok == IDENTIFIER || right_tok == MACRO || right_tok <= WHILE_KW) {
      accum_string_string(right_val);
    } else if (right_tok == INTEGER) {
      accum_string_integer(-right_val);
    } else {
      putstr("left_tok="); putint(left_tok); putstr(", right_tok="); putint(right_tok); putchar('\n');
      // show identifier/macro string
      putstr("left="); putstr(STRING_BUF(left_val)); putchar('\n');
      syntax_error("cannot paste an identifier with a non-identifier or non-negative integer");
    }

    val = end_ident();
    tok = heap[val+2]; // The kind of the identifier
  } else if (left_tok == INTEGER) {
    if (right_tok == INTEGER) {
      val = -paste_integers(-left_val, -right_val);
    } else if (right_tok == IDENTIFIER || right_tok == MACRO || right_tok <= WHILE_KW) {
      begin_string();
      accum_string_integer(-left_val);
      accum_string_string(right_val);

      val = end_ident();
      tok = heap[val+2]; // The kind of the identifier
    } else {
      putstr("left_tok="); putint(left_tok); putstr(", right_tok="); putint(right_tok); putchar('\n');
      syntax_error("cannot paste an integer with a non-integer");
    }
  } else {
    putstr("left_tok="); putint(left_tok); putstr(", right_tok="); putint(right_tok); putchar('\n');
    syntax_error("cannot paste a non-identifier or non-integer");
  }
}

void get_tok() {

#ifdef SH_INCLUDE_C_CODE
  int prev_char_buf_ix = declaration_char_buf_ix;
  // Save the cursor in a local variable so we can restore it when the token is
  // masked off. Not using the last_tok_char_buf_ix global because get_tok can
  // be called recursively by handle_preprocessor_directive.
  int prev_last_tok_char_buf_ix = declaration_char_buf_ix;
#endif

#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  int prev_tok_line_number = line_number;
  int prev_tok_column_number = column_number;
#endif

  // This outer loop is used to skip over tokens removed by #ifdef/#ifndef/#else
  do {
#ifdef SH_INCLUDE_C_CODE
    declaration_char_buf_ix = prev_char_buf_ix; // Skip over tokens that are masked off
#endif

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
        if (macro_tok_lst != 0 && car(car(macro_tok_lst)) == HASH_HASH) {
          if (tok == MACRO || tok == MACRO_ARG) {
            // If the token is a macro or macro arg, it must be expanded before pasting
            macro_tok_lst = cdr(macro_tok_lst); // We consume the ## token
            paste_last_token = true;
          } else {
            // macro_tok_lst is not empty because read_macro_tokens checked for trailing ##
            macro_tok_lst = cdr(macro_tok_lst); // Skip the ##
            paste_tokens(tok, val);
            break;
          }
        } else if (macro_tok_lst == 0 && paste_last_token) { // We finished expanding the left-hand side of ##
          if (macro_stack_ix == 0) {
            // If we are not in a macro expansion, we can't paste the last token
            // This should not happen if the macro is well-formed, which is
            // checked by read_macro_tokens.
            syntax_error("## cannot appear at the end of a macro expansion");
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
        } else if (tok == MACRO_ARG && expand_macro_arg) {
          play_macro(get_macro_arg(val), 0); // Play the tokens of the macro argument
          continue;
        } else if (tok == '#') { // Stringizing!
          stringify();
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

        // skip whitespace, detecting when it is at start of line.
        // When skip_newlines is false, produces a '\n' token whenever it
        // encounters whitespace containing at least a newline.
        // This condenses multiple newlines into a single '\n' token and serves
        // to end the current preprocessor directive.

        tok = 0; // Reset the token
        while (0 <= ch && ch <= ' ') {
          if (ch == '\n') tok = ch;
          get_ch();
        }

        if (tok == '\n' && !skip_newlines) {
          // If the newline is followed by a #, the preprocessor directive is
          // handled in the next iteration of the loop.
          break;
        }

        // will continue while (1) loop
      }

      // detect '#' at start of line, possibly preceded by whitespace
      else if (tok == '\n' && ch == '#') {
        tok = 0; // Consume the newline so handle_preprocessor_directive's get_tok doesn't re-enter this case
        handle_preprocessor_directive();
        // will continue while (1) loop
      }

      else if (('a' <= ch && ch <= 'z') ||
               ('A' <= ch && ch <= 'Z') ||
               (ch == '_')) {

        get_ident();

        if (tok == MACRO) {
          // We only expand in ifdef true blocks and if the expander is enabled.
          // Since this is the "base case" of the macro expansion, we don't need
          // to disable the other places where macro expansion is done.
          if (if_macro_mask && expand_macro) {
            if (attempt_macro_expansion(val)) {
              continue;
            }
            break;
          }
        }
        break;
      } else if ('0' <= ch && ch <= '9') {

        val = '0' - ch;

        get_ch();

        if (val == 0) { // val == 0 <=> ch == '0'
          if (ch == 'x' || ch == 'X') {
            get_ch();
            val = 0;
            if (accum_digit(16)) {
              while (accum_digit(16));
            } else {
              syntax_error("invalid hex integer -- it must have at least one digit");
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
          syntax_error("unterminated character literal");
        }

        get_ch();

        tok = CHARACTER;

        break;

      } else if (ch == '\"') {

        get_ch();

        begin_string();
        accum_string_until('\"');

        val = end_string();
        tok = STRING;

        break;

      } else {

        tok = ch; // fallback for single char tokens

        if (ch == '/') {

          get_ch();
          if (ch == '*') {
            get_ch();
            tok = ch; // remember previous char, except first one
            while ((tok != '*' || ch != '/') && ch != EOF) {
              tok = ch;
              get_ch();
            }
            if (ch == EOF) {
              syntax_error("unterminated comment");
            }
            get_ch();
            // will continue while (1) loop
          } else if (ch == '/') {
            while (ch != '\n' && ch != EOF) {
              get_ch();
            }
            // will continue while (1) loop
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

        } else if (ch == '.') {
          get_ch();
          if (ch == '.') {
            get_ch();
            if (ch == '.') {
              get_ch();
              tok = ELLIPSIS;
            } else {
              syntax_error("invalid token");
            }
          } else {
            tok = '.';
          }
          break;
        } else if (ch == '~' || ch == '.' || ch == '?' || ch == ',' || ch == ':' || ch == ';' || ch == '(' || ch == ')' || ch == '[' || ch == ']' || ch == '{' || ch == '}') {

          tok = ch;

          get_ch();

          break;

        } else if (ch == '\\') {
          get_ch();

          if (ch == '\n') { // Continues with next token
            get_ch();
          } else {
            putstr("ch="); putint(ch); putchar('\n');
            syntax_error("unexpected character after backslash");
          }
        } else {
          putstr("ch="); putint(ch); putchar('\n');
          syntax_error("invalid token");
        }
      }
    }
  } while (!if_macro_mask);

#ifdef SH_INCLUDE_C_CODE
  last_tok_char_buf_ix = prev_last_tok_char_buf_ix - 1;
#endif

#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  last_tok_line_number = prev_tok_line_number;
  last_tok_column_number = prev_tok_column_number;
#endif
}

// parser
#if defined DEBUG_CPP || defined DEBUG_EXPAND_INCLUDES || defined NICE_ERR_MSG
#include "debug.c"
#endif

#define parse_error(msg, token) parse_error_internal(msg, token, __FILE__, __LINE__)

void parse_error_internal(char * msg, int token, char * file, int line) {

#ifdef NICE_ERR_MSG
  #define ANSI_RED     "\x1b[31m"
  #define ANSI_GREEN   "\x1b[32m"
  #define ANSI_YELLOW  "\x1b[33m"
  #define ANSI_BLUE    "\x1b[34m"
  #define ANSI_MAGENTA "\x1b[35m"
  #define ANSI_CYAN    "\x1b[36m"
  #define ANSI_RESET   "\x1b[0m"

  //Error header
  putstr(ANSI_RED"Error occurred while parsing ");
  putstr(ANSI_GREEN"\"");
  putstr(include_stack->filepath);
  putstr("\""ANSI_RESET"\n");

  //Error message
  putstr("  Message: "ANSI_YELLOW);
  putstr(msg);
  putstr(ANSI_RESET"\n");

  //Error token
  putstr("  Offending Token: "ANSI_YELLOW);
  print_tok_type(token);
  putstr(ANSI_RESET"\n");

  //Error location
  putstr("  Location: "ANSI_GREEN);
  putstr(include_stack->filepath);
  putchar(':');
  putint(last_tok_line_number);
  putchar(':');
  putint(last_tok_column_number);
  putstr(ANSI_RESET"\n");
#else
  putstr(msg);
#endif

#ifdef DEBUG_SHOW_ERR_ORIGIN
  putstr("Note, error emitted from ");
  putstr(file);
  putstr(" line ");
  putint(line);
  putstr("\n");
#endif

  exit(1);
}


void expect_tok(int expected_tok) {
  if (tok != expected_tok) {
#ifdef NICE_ERR_MSG
    putstr("expected tok="); print_tok_type(expected_tok);
    putstr("\ncurrent tok="); print_tok_type(tok); putchar('\n');
#else
    putstr("expected tok="); putint(expected_tok);
    putstr("\ncurrent tok="); putint(tok); putchar('\n');
#endif
    parse_error("unexpected token", tok);
  }
  get_tok();
}

ast parse_comma_expression();
ast parse_cast_expression();
ast parse_compound_statement();
ast parse_conditional_expression();
ast parse_enum();
ast parse_struct_or_union(int struct_or_union_tok);

ast parse_type() {

  int type_kw = 0;

  while (1) {
    if (tok == INT_KW || tok == SHORT_KW || tok == LONG_KW || tok == SIGNED_KW) {
      if (type_kw != 0 && type_kw != INT_KW) parse_error("inconsistent type", tok);
      type_kw = INT_KW;
      get_tok();
    } else if (tok == CHAR_KW) {
      if (type_kw != 0) parse_error("inconsistent type", tok);
      type_kw = CHAR_KW;
      get_tok();
    } else if ((tok == UNSIGNED_KW) || (tok == FLOAT_KW) || (tok == DOUBLE_KW)) {
      parse_error("unsupported type", tok);
    } else if (tok == VOID_KW) {
      if (type_kw != 0) parse_error("inconsistent type", tok);
      type_kw = VOID_KW;
      get_tok();
    } else if (tok == CONST_KW) {
      get_tok(); // ignore const
    } else if (tok == ENUM_KW) {
      if (type_kw != 0) parse_error("inconsistent type", tok);
      return parse_enum();
    } else if (tok == STRUCT_KW || tok == UNION_KW) {
      if (type_kw != 0) parse_error("inconsistent type", tok);
      return parse_struct_or_union(tok);
    } else if (tok == TYPE) {
      // Look in types table. It's a type, not a type_kw, but we reuse the variable
      type_kw = heap[val + 3]; // For TYPE tokens, the tag is the type
      get_tok();
      return type_kw;
    } else {
      break;
    }
  }

  if (type_kw == 0) {
    parse_error("type expected", tok);
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

int parse_stars_for_type(int type) {
  int stars = parse_stars();

  // We don't want to mutate types that are typedef'ed, so making a copy of the type obj
  if (stars != 0) {
    type = clone_ast(type);
    set_val(type, stars);
  }

  return type;
}

//defining a const after the * is valid c, ie
//   const int * const foo;
void ignore_optional_const() {
    if(tok == CONST_KW) {
        //skip the const
        get_tok();
    }
}

int parse_type_with_stars() {
  int type = parse_stars_for_type(parse_type());
  ignore_optional_const();
  return type;
}

int is_type_starter(int tok) {
  return tok == INT_KW || tok == CHAR_KW || tok == SHORT_KW || tok == LONG_KW || tok == SIGNED_KW // Supported types
      || tok == UNSIGNED_KW || tok == FLOAT_KW || tok == DOUBLE_KW || tok == VOID_KW  // Unsupported types
      || tok == TYPE                                                                  // User defined types
      || tok == CONST_KW                                                              // Type attributes
      || tok == ENUM_KW || tok == STRUCT_KW || tok == UNION_KW;                       // Enum, struct, union
}

ast parse_enum() {
  ast name;
  ast ident;
  ast result = 0;
  ast tail;
  ast value = 0;
  int next_value = 0;

  expect_tok(ENUM_KW);

  if (tok == IDENTIFIER || tok == TYPE) {
    // When the enum keyword is used with an identifier that's typedefed, the typedef is ignored.
    name = new_ast0(IDENTIFIER, val);
    get_tok();
  } else {
    name = 0;
  }

  // Note: The parser doesn't distinguish between a reference to an enum type and a declaration.
  // If child#2 is 0, it's either a reference to a type or a forward declaration.
  if (tok == '{') {
    get_tok();

    while (tok != '}') {
      if (tok != IDENTIFIER) {
        parse_error("identifier expected", tok);
      }
      ident = new_ast0(IDENTIFIER, val);
      get_tok();

      if (tok == '=') {
        get_tok();

        if (tok != INTEGER) parse_error("integer expected", tok);
        value = new_ast0(INTEGER, val);
        next_value = val - 1; // Next value is the current value + 1, but val is negative
        get_tok(); // skip
      } else {
        value = new_ast0(INTEGER, next_value);
        next_value -= 1;
      }

      if (result == 0) {
        result = new_ast3(',', ident, value, 0);
        tail = result;
      } else {
        set_child(tail, 2, new_ast3(',', ident, value, 0));
        tail = get_child(tail, 2);
      }

      if (tok == ',') {
        get_tok();
      } else {
        break;
      }
    }

    expect_tok('}');

  }

  return new_ast3(ENUM_KW, 0, name, result); // 0 is number of stars
}

ast parse_struct_or_union(int struct_or_union_tok) {
  ast name;
  ast ident;
  ast type;
  ast result = 0;
  ast tail;
  bool ends_in_flex_array = false;

  expect_tok(struct_or_union_tok);

  if (tok == IDENTIFIER || tok == TYPE) {
    // When the struct/union keyword is used with an identifier that's typedefed, the typedef is ignored.
    name = new_ast0(IDENTIFIER, val);
    get_tok();
  } else {
    name = 0; // Unnamed struct
  }

  // Note: The parser doesn't distinguish between a reference to a struct/union type and a declaration.
  // If child#2 is 0, it's either a reference to a type or a forward declaration.
  if (tok == '{') {
    get_tok();

    while (tok != '}') {
      if (!is_type_starter(tok)) parse_error("type expected in struct declaration", tok);
      if (ends_in_flex_array)    parse_error("flexible array member must be last", tok);

      type = parse_type_with_stars();

      if (get_val(type) == 0 && get_op(type) == VOID_KW)
        parse_error("variable with void type", tok);

      ident = 0; // Anonymous struct
      if (tok == IDENTIFIER) {
      ident = new_ast0(IDENTIFIER, val);
      get_tok();

      if (tok == '[') { // Array
        get_tok();
          if (tok == ']') {
            if (struct_or_union_tok != STRUCT_KW) parse_error("flexible array member must be in a struct", tok);
            ends_in_flex_array = true;
            val = 0; // Flex array are arrays with no size, using 0 for now
            type = new_ast2('[', new_ast0(INTEGER, 0), type);
            get_tok();
          } else if (tok == INTEGER) {
        type = new_ast2('[', new_ast0(INTEGER, -val), type);
        get_tok();
        expect_tok(']');
          } else {
            parse_error("array size must be an integer constant", tok);
          }

        }
      } else if (get_op(type) != STRUCT_KW && get_op(type) != UNION_KW) {
        parse_error("Anonymous struct/union member must have be a struct or union type", tok);
      }

      expect_tok(';');

      if (result == 0) {
        result = new_ast3(',', ident, type, 0);
        tail = result;
      } else {
        set_child(tail, 2, new_ast3(',', ident, type, 0));
        tail = get_child(tail, 2);
      }
    }

    expect_tok('}');

  }

  return new_ast3(struct_or_union_tok, 0, name, result); // 0 is number of stars
}

ast parse_param_decl() {

  ast type;
  int name;
  ast result = 0;

  if (is_type_starter(tok)) {
    type = parse_type_with_stars();
    name = val;
    expect_tok(IDENTIFIER);
    if (get_val(type) == 0 && get_op(type) == VOID_KW) parse_error("variable with void type", tok);
    result = new_ast3(VAR_DECL, name, type, 0);
  } else if (tok == IDENTIFIER) {
    // Support K&R param syntax in function definition
    name = val;
    expect_tok(IDENTIFIER);
    type = new_ast0(INT_KW, 0);
    result = new_ast3(VAR_DECL, name, type, 0);
  } else if (tok == ELLIPSIS) {
    // ignore ELLIPSIS nodes for now
    get_tok();
  }

  return result;
}

int parse_param_list() {
  ast decl = parse_param_decl();
  ast result = 0;
  ast tail;
  if (decl != 0) {
    result = new_ast2(',', decl, 0);
    tail = result;

    while (tok == ',') {
      get_tok();
      decl = parse_param_decl();
      if (decl == 0) { break; }

      decl = new_ast2(',', decl, 0);
      set_child(tail, 1, decl);
      tail = decl;
    }
  }

  return result;
}

// Note: Uses a simplified syntax for definitions
ast parse_definition(int local) {

  ast type;
  ast init;
  int name;
  ast params;
  ast body;
  ast this_type;
  ast result = 0;
  ast tail = 0;
  ast current_declaration;

  //static can be skipped for global definitions without affecting semantics
  if(!local && tok == STATIC_KW) {
    get_tok();
  }

  if (is_type_starter(tok)) {
    type = parse_type();

    // global enum/struct/union declaration
    if (tok == ';') {
      if (get_op(type) != ENUM_KW && get_op(type) != STRUCT_KW && get_op(type) != UNION_KW) {
        parse_error("enum/struct/union declaration expected", tok);
      }
      get_tok();
      return type;
    }

    while (1) {

      this_type = parse_stars_for_type(type);
      ignore_optional_const();

      name = val;

      expect_tok(IDENTIFIER);

      if (tok == '(') {

        if (local) {
          parse_error("function declaration only allowed at global level", tok);
        }

        get_tok();

        params = parse_param_list();

        expect_tok(')');

        if (tok == ';') {
          // forward declaration. Body == -1
          body = -1;
          get_tok();
        } else {
          body = parse_compound_statement();
        }

        return new_ast4(FUN_DECL, name, this_type, params, body);

      } else {

        if (get_val(this_type) == 0 && get_op(this_type) == VOID_KW) {
          parse_error("variable with void type", tok);
        }

        if (tok == '[') {
          // if (local) {
          //   syntax_error("array declaration only allowed at global level");
          // }
          get_tok();
          if (tok == INTEGER) {
            this_type = new_ast2('[', new_ast0(INTEGER, -val), this_type);
            get_tok();
          } else {
            parse_error("array size must be an integer constant", tok);
          }

          expect_tok(']');
        }

        init = 0;

        if (tok == '=') {
          get_tok();
            init = parse_conditional_expression();
        }
        current_declaration = new_ast3(VAR_DECL, name, this_type, init); // Create a new declaration

        if(result == 0) { // First declaration
          result = new_ast2(',', current_declaration, 0);
          tail = result; // Keep track of the last declaration
        } else {
          set_child(tail, 1, new_ast2(',', current_declaration, 0)); // Link the new declaration to the last one
          tail = get_child(tail, 1); // Update the last declaration
        }

        if (tok == ';') {
          get_tok();
          break;
        } else if (tok == ',') {
          get_tok();
          continue; // Continue to the next declaration
        } else {
          parse_error("';' or ',' expected", tok);
        }
      }
    }
    return new_ast1(VAR_DECLS, result);
  } else if (tok == TYPEDEF_KW) {
    // When parsing a typedef, the type is added to the types table.
    // This is so the parser can determine if an identifier is a type or not.
    // This implementation is not completely correct, as an identifier that was
    // typedef'ed can also be used as a variable name, but TCC doesn't do that so
    // it should be fine for now.
    //
    // When we want to implement typedef correctly, we'll want to tag
    // identifiers as typedef'ed and have the typedef be scoped to the block
    // it was defined in (global or in function).
    get_tok();
    type = parse_type_with_stars();
    if (tok != IDENTIFIER) { parse_error("identifier expected", tok); }

#ifdef sh
    // If the struct/union/enum doesn't have a name, we give it the name of the typedef.
    // This is not correct, but it's a limitation of the current shell backend where we
    // need the name of a struct/union/enum to compile sizeof and typedef'ed structures
    // don't always have a name.
    if (get_op(type) == STRUCT_KW || get_op(type) == UNION_KW || get_op(type) == ENUM_KW) {
      if (get_child(type, 1) != 0 && get_val(get_child(type, 1)) != val) {
        syntax_error("typedef name must match struct/union/enum name");
      }
      set_child(type, 1, new_ast0(IDENTIFIER, val));
    }
#endif

    heap[val + 2] = TYPE;
    heap[val + 3] = type;
    result = new_ast2(TYPEDEF_KW, val, type);
    get_tok();
    expect_tok(';');
    return result;
  } else {
    parse_error("unknown decl: type expected", tok);
    return result;
  }
}

ast parse_parenthesized_expression() {

  ast result;

  expect_tok('(');

  result = parse_comma_expression();

  expect_tok(')');

  return new_ast1(PARENS, result);
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
      result = cons(get_val(result), 0); // Result is now a list of string values
      tail = result;
      while (tok == STRING) {
        set_cdr(tail, cons(val, 0));
        tail = cdr(tail);
        get_tok();
      }

      // Unpack the list of strings into a single string
      begin_string();

      while (result != 0) {
        accum_string_string(car(result));
        result = cdr(result);
      }

      result = new_ast0(STRING, end_string());
    }

  } else if (tok == '(') {

    result = parse_parenthesized_expression();

  } else {
    parse_error("identifier, literal, or '(' expected", tok);
    return 0;
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

      get_tok();
      if (tok != IDENTIFIER) {
        parse_error("identifier expected", tok);
      }
      result = new_ast2('.', result, new_ast0(IDENTIFIER, val));
      get_tok();

    } else if (tok == ARROW) {

      get_tok();
      if (tok != IDENTIFIER) {
        parse_error("identifier expected", tok);
      }
      result = new_ast2(ARROW, result, new_ast0(IDENTIFIER, val));
      get_tok();

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

  if (tok == PLUS_PLUS) {

    get_tok();
    result = parse_unary_expression();
    result = new_ast1(PLUS_PLUS_PRE, result);

  } else if (tok == MINUS_MINUS) {

    get_tok();
    result = parse_unary_expression();
    result = new_ast1(MINUS_MINUS_PRE, result);

  } else if (tok == '&' || tok == '*' || tok == '+' || tok == '-' || tok == '~' || tok == '!') {

    op = tok;
    get_tok();
    result = parse_cast_expression();
    result = new_ast1(op, result);

  } else if (skip_newlines && tok == SIZEOF_KW) { // only parse sizeof if we're not in a #if expression

    get_tok();
    if (tok == '(') {
      get_tok();
      result = parse_type_with_stars();
      expect_tok(')');
    } else {
      result = parse_unary_expression();
    }
    result = new_ast1(SIZEOF_KW, result);

  } else if (!skip_newlines && tok == IDENTIFIER && val == DEFINED_ID) { // Parsing a macro

    get_tok_macro();
    if (tok == '(') {
      get_tok_macro();
      result = new_ast2('(', new_ast0(IDENTIFIER, DEFINED_ID), tok);
      get_tok_macro();
      expect_tok(')');
    } else if (tok == IDENTIFIER || tok == MACRO) {
      result = new_ast2('(', new_ast0(IDENTIFIER, DEFINED_ID), tok);
      get_tok_macro();
    } else {
      parse_error("identifier or '(' expected", tok);
      return 0;
    }

  } else {
    result = parse_postfix_expression();
  }

  return result;
}

ast parse_cast_expression() {
  int tokens = 0;
  ast result;
  ast type;

  if (tok == '(') {
    // Ideally, we'd parse as many ( as needed, but then we would have to
    // backtrack when the first parenthesis is for a parenthesized expression
    // and not a cast.
    // I think we could write a version of parse_parenthesized_expression that
    // already has the first parenthesis consumed. It would be called when
    // after parsing the cast and cast expression, there are still parenthesis
    // to close, but I'm not sure how we could create the AST since it's all
    // very top down and that would flip the order of the AST creation.

    // Concretely, this means we can't parse cast expressions where the type
    // is wrapped in parenthesis, like in the following example:
    // (((char *)) var)
    // But that should be ok for TCC.
    get_tok();

    if (is_type_starter(tok)) {
      type = parse_type_with_stars();

      expect_tok(')');
      result = new_ast2(CAST, type, parse_cast_expression());
      return result;
    } else {
      // We need to put the current token and '(' back on the token stream.
      tokens = cons(cons(tok, val), 0);
      play_macro(tokens, 0);
      tok = '(';
      val = 0;
    }
  }

  return parse_unary_expression();
}

ast parse_multiplicative_expression() {

  ast result = parse_cast_expression();
  ast child;
  int op;

  while (tok == '*' || tok == '/' || tok == '%') {

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

  while (tok == '+' || tok == '-') {

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

  while (tok == LSHIFT || tok == RSHIFT) {

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

  while (tok == '<' || tok == '>' || tok == LT_EQ || tok == GT_EQ) {

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

  while (tok == EQ_EQ || tok == EXCL_EQ) {

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

  if (   tok == '='       || tok == PLUS_EQ   || tok == MINUS_EQ
      || tok == STAR_EQ   || tok == SLASH_EQ  || tok == PERCENT_EQ
      || tok == LSHIFT_EQ || tok == RSHIFT_EQ || tok == AMP_EQ
      || tok == CARET_EQ  || tok == BAR_EQ) {

    op = tok;
    get_tok();
    child = parse_assignment_expression();
    result = new_ast2(op, result, child);

  }

  return result;
}

ast parse_comma_expression() {

  ast result = parse_assignment_expression();

  if (tok == ',') { // "comma expressions" without , don't need to be wrapped in a comma node
    get_tok();
    result = new_ast2(',', result, 0);
    set_child(result, 1, parse_comma_expression());
  }

  return result;
}

ast parse_comma_expression_opt() {

  ast result;

  if (tok == ':' || tok == ';' || tok == ')') {
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

    if (tok == ':' && start_tok != '(' && get_op(result) == IDENTIFIER) {

      get_tok(); // Skip :

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

  // TODO: Simplify this
  if (tok != '}' && tok != EOF) {
    if (is_type_starter(tok)) {
      child1 = parse_definition(1);
    } else {
      child1 = parse_statement();
    }
    result = new_ast2('{', child1, 0);
    tail = result;
    while (tok != '}' && tok != EOF) {
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

#if !(defined DEBUG_CPP) && !(defined DEBUG_EXPAND_INCLUDES) && !(defined DEBUG_PARSER) && !(defined DEBUG_GETCHAR)
#ifdef sh
#include "sh.c"
#endif

#ifdef target_i386_linux
#include "x86.c"
#endif

#ifdef target_x86_64_linux
#include "x86.c"
#endif

#ifdef target_x86_64_mac
#include "x86.c"
#endif

#ifdef arm
#include "arm.c"
#endif
#endif

//-----------------------------------------------------------------------------

int main(int argc, char **argv) {
  int i;
  ast decl;

  init_ident_table();

  init_pnut_macros();

  for (i = 1; i < argc; i += 1) {
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {
        case 'D':
          init_ident(MACRO, argv[i] + 2);
          break;

        case 'U':
          init_ident(IDENTIFIER, argv[i] + 2);
          break;

        case 'I':
          if (include_search_path != 0) {
            fatal_error("only one include path allowed");
          }
          include_search_path = argv[i] + 2;
          break;

        default:
          putstr("Option "); putstr(argv[i]); putchar('\n');
          fatal_error("unknown option");
          break;
      }
    } else {
      // Options that don't start with '-' are file names
      include_file(argv[i], 0);
    }
  }

  if (fp == 0) {
    putstr("Usage: "); putstr(argv[0]); putstr(" <filename>\n");
    fatal_error("no input file");
  }

  ch = '\n';

#if defined DEBUG_GETCHAR // Read input
  while (ch != EOF) {
    get_ch();
  }
#elif defined DEBUG_EXPAND_INCLUDES || defined DEBUG_CPP // Tokenize input, output tokens
  get_tok();

  while (tok != EOF) {
    skip_newlines = false; // Don't skip newlines so print_tok knows where to break lines
#if defined DEBUG_CPP
    print_tok(tok, val);
#endif
    get_tok();
  }
#elif defined DEBUG_PARSER // Parse input, output nothing
    get_tok();
  while (tok != EOF) {
    decl = parse_definition(0);
  }
#else
  codegen_begin();
  get_tok();
  while (tok != EOF) {
    decl = parse_definition(0);
#ifdef SH_INCLUDE_C_CODE
    output_declaration_c_code(get_op(decl) == '=' | get_op(decl) == VAR_DECLS);
#endif
    codegen_glo_decl(decl);
  }
  codegen_end();
#endif

  return 0;
}
