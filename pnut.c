// Those includes are parsed by pnut but ignored
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h> // for intptr_t
#include <fcntl.h> // for open
#include <unistd.h> // for write

// =========================== configuration options ===========================
//
// Pnut has many compilation options to change the features supported by pnut-sh
// and pnut-exe, and the code generation.
// The reason for so many options is that pnut is really 2 different compilers
// (pnut-sh and pnut-exe) sharing the same compiler frontend, with each compiler
// having to accomplish 2 opposite goals:
//
//  1. Be as small and simple as possible to bootstrap a C-like language from a
//     POSIX shell implementation. Importantly, this applies to both the
//     compiled shell scripts and to the compiler code itself.
//
//  2. Compile real world programs, such as TCC, requiring a large subset of C99
//     language.
//     Non-bootstrap users of pnut also care about user experience, such as
//     informative error messages with line numbers, a nice CLI, a fully
//     featured built-in libc and support for more advanced C features.
//
// We resolve this tension by having a shared codebase and preprocessor options
// to enable or disable features as needed. Fortunately, pnut's reader and
// tokenizer are fast compared to the rest of the compilation, so having a
// single codebase with large blocks of inactive code doesn't impact performance
// all that much.
//
// However, the preprocessor directives can make the code slightly harder to
// read and maintain. To make reviewing the code easier, a simple
// preprocessor-like tool removing inactive code blocks is provided in the
// "tools" directory (TODO).
//
// Since most options are orthogonal, the number of possible combinations is
// very large. To avoid combinatorial explosion of testing and maintenance,
// we define a few "profiles" that cover the common use cases of pnut:
//
// - PNUT_BOOTSTRAP:
//    Profile used to generate pnut-sh.sh and to bootstrap pnut-exe from a
//    POSIX shell. This profile minimizes the code size and complexity of the
//    compilers as much as possible, supporting only the features strictly
//    needed to bootstrap pnut.
//
// - (Default):
//    General purpose profile used to build pnut-exe for real world usage. This
//    profile strikes a balance between features, usability and code size.
//
// - BOOTSTRAP_TCC:
//    Profile used to bootstrap TCC from pnut-exe. This profile enables partial
//    support for a few more features needed to compile TCC. These features are
//    not implemented correctly, and so are not enabled in the general purpose
//    profile.
//
// Additionally, if compilation time and code size are not a concern, or to help
// debugging, the NICE_UX and SAFE_MODE options can be enabled to turn on extra
// runtime checks and better error messages.
//
// =============================================================================

// Pnut-sh specific options
#ifdef sh

  // Toggles parsing literals with their base (octal, decimal or hexadecimal).
  // This is used by the shell code generator to output the literal in the correct base.
  #define PARSE_NUMERIC_LITERAL_WITH_BASE

  #ifdef PNUT_BOOTSTRAP
    #define ALLOW_RECURSIVE_MACROS
    #define SH_MINIMAL_RUNTIME
    #define SH_INCLUDE_ALL_ALPHANUM_CHARACTERS
  #else
    // Enable all C features for general pnut usage
    #define SUPPORT_ALL_C_FEATURES
    // Pnut-sh specific features
    #define SH_SUPPORT_SHELL_INCLUDE
    #define SH_SUPPORT_ADDRESS_OF
    #define SH_OPTIMIZE_LONG_LINES
  #endif

  // Shell code generation options
  #ifndef SH_INLINE_PRINTF_NOT
  // Inline printf calls with literal string for smaller and faster code
  #define SH_INLINE_PRINTF
  #endif
  #ifndef SH_INLINE_PUTCHAR_NOT
  // Inline putchar calls for smaller and faster code
  #define SH_INLINE_PUTCHAR
  #endif
  // Inline exit calls for smaller code
  #define SH_INLINE_EXIT

  #ifndef SH_SAVE_VARS_WITH_SET
  // Pick calling convention implementation. By default, use let/endlet.
  // When SH_SAVE_VARS_WITH_SET is set, local variables are saved using "set"
  // builtin, which results in faster code but makes the calling convention
  // implementation much harder to read.
  #define SH_INITIALIZE_PARAMS_WITH_LET
  #endif

  #ifndef SH_OPTIMIZE_CONSTANT_PARAMS_NOT
  // Use positional parameter for const function parameters.
  // Results in smaller and faster code (as it completely removes the cost of
  // constant function parameters), but can make it harder to track variable
  // usage in the generated code.
  // This optimization is preferable to SH_SAVE_VARS_WITH_SET.
  #define SH_OPTIMIZE_CONSTANT_PARAMS
  #endif

  // Built-in runtime library options
  #ifndef RT_FREE_UNSETS_VARS_NOT
  // Make free unset all variables associated with the freed object
  #define RT_FREE_UNSETS_VARS
  #endif

  #ifndef RT_NO_INIT_GLOBALS_NOT
  // Skips the zero-initialization of memory allocated values.
  // Prevents the shell environment from being polluted with a large number of
  // variables when allocating large data structures.
  #define RT_NO_INIT_GLOBALS
  #endif

  #ifndef RT_USE_LOOKUP_TABLE_NOT
  // Use a lookup table for char->int for a faster conversion
  #define RT_USE_LOOKUP_TABLE
  #endif

  #ifdef SH_OPTIMIZE_LONG_LINES_NOT
  // Disable long line optimization
  #undef SH_OPTIMIZE_LONG_LINES
  #endif

  // Disabled options:
  // Include the C code as comment along with the generated shell code
  // #define SH_INCLUDE_C_CODE

  // Replace character literal with character code instead of character variables.
  // Results in smaller and faster code, but with magic numbers in the output.
  // #define SH_INLINE_CHAR_LITERAL

  // Remove the `set -u` directive in the generated shell code.
  // This can be useful for programs that must allocate large amounts of
  // zero-initialized memory, with incurring the overhead of actually
  // initializing it to zero.
  // This option is generally used together with RT_NO_INIT_GLOBALS, which skips
  // the zero-initialization of memory. RT_UNSAFE_HEAP then allows access to
  // this uninitialized memory without triggering unbound variable errors. This
  // is made possible by the fact that uninitialized variables are always
  // treated as zero in arithmetic expansions.
  // #define RT_UNSAFE_HEAP

  // Use a more compact but slower implementation of the runtime library.
  // #define RT_COMPACT

  // Include the complete runtime library in the generated shell code,
  // regardless of whether features are used or not.
  // #define INCLUDE_ALL_RUNTIME

  // Only emit shell code output at the end of the compilation.
  // This is used to evaluate the negative performance impact of allocating
  // There is no practical reason to enable this option.
  // #define ONE_PASS_GENERATOR_NO_EARLY_OUTPUT

  // Make sure we don't use the long line optimization when RT_COMPACT is on
  #ifdef RT_COMPACT
  #undef SH_OPTIMIZE_LONG_LINES
  #undef RT_USE_LOOKUP_TABLE
  #endif

#elif defined(target_i386_linux) || defined (target_x86_64_linux) || defined (target_x86_64_mac)

  // Parse numeric literals with their suffix (U, L, UL, etc).
  #define PARSE_NUMERIC_LITERAL_SUFFIX

  // Varargs functions are always supported because the built-in libc uses them.
  #define SUPPORT_VARIADIC_FUNCTIONS

  // Support 64 bit literals on 64 bit platforms
  #if defined (target_x86_64_linux) || defined (target_x86_64_mac)
    #define SUPPORT_64_BIT_LITERALS
  #endif

  #ifdef PNUT_BOOTSTRAP
    #define ALLOW_RECURSIVE_MACROS
  #else
    // Enable all C features for general pnut usage
    #define SUPPORT_ALL_C_FEATURES
  #endif

  #ifdef BOOTSTRAP_TCC
    #define BOOTSTRAP_LONG
    #define NO_BUILTIN_LIBC
  #endif

  // Disabled options:

  // Output machine code as soon as possible, typically after each top level
  // declaration, reducing memory usage significantly, but generating slightly
  // worse code.
  // #define ONE_PASS_GENERATOR

  // Use brk syscall for memory allocation instead of mmap.
  // #define USE_BRK_SYSCALL

  // Add the PNUT_INLINE_INTERRUPT() special function that inserts an interrupt
  // check at the call site, useful for debugging.
  // #define ENABLE_PNUT_INLINE_INTERRUPT

  // Poor man's debug info: adds the name of functions before their entry points.
  // #define ADD_DEBUG_INFO

  // Treats undefined labels as runtime errors instead of compile-time errors.
  // This catches functions that are declared but not defined.
  // #define UNDEFINED_LABELS_ARE_RUNTIME_ERRORS

  // Use stack allocation for global variables instead of heap allocation.
  // #define USE_STACK_FOR_GLOBALS

#else
  // Frontend-only variants of pnut (e.g. for running reader, tokenizer or parser)
  // Options that turns Pnut into a C preprocessor or some variant of it
  // DEBUG_GETCHAR: Read and print the input character by character.
  // DEBUG_CPP: Run preprocessor like gcc -E. This can be useful for debugging the preprocessor.
  // DEBUG_EXPAND_INCLUDES: Reads the input file and includes the contents of the included files.
  // DEBUG_PARSER: Runs the tokenizer on the input. Outputs nothing.

  // Enable all C features in the frontend
  #define SUPPORT_ALL_C_FEATURES
#endif


#ifdef SUPPORT_ALL_C_FEATURES
  #define FULL_CLI_OPTIONS
  #define FULL_PREPROCESSOR_SUPPORT
  #define SUPPORT_COMPLEX_INITIALIZER
  #define SUPPORT_DO_WHILE
  #define SUPPORT_GOTO
  #define SUPPORT_SIZEOF
  #define SUPPORT_STRUCT_UNION
  #define SUPPORT_TYPE_SPECIFIERS
  #define SUPPORT_VARIADIC_FUNCTIONS
#endif

#if defined(NICE_UX) || defined(SAFE_MODE)
  #define INCLUDE_LINE_NUMBER_ON_ERROR
  #define NICE_ERR_MSG
#endif

// Disabled options:
// Support skip line continuations
// #define SUPPORT_LINE_CONTINUATION

// Print memory usage statistics at the end of the program.
// #define PRINT_MEMORY_STATS

// ===================== Compatibility macros and typedefs =====================
#ifdef NO_TERNARY_SUPPORT
// M2-Planet doesn't support ternary operator.
// Ternary operator can be implemented using arithmetic operations.
// Note that unlike the standard ternary operator, both sides are always
// evaluated, and the condition expression is evaluated twice.
#define TERNARY(cond, if_true, if_false) (((cond) == 0) * (if_false) + ((cond) != 0) * (if_true))
#else
#define TERNARY(cond, if_true, if_false) ((cond) ? (if_true) : (if_false))
#endif

#ifdef PNUT_CC
// When bootstrapping pnut, intptr_t is not defined.
// On 64 bit platforms, intptr_t is a long long int.
// On 32 bit (including shells) platforms, intptr_t is an int.
#if defined(PNUT_EXE_64)
typedef long long int intptr_t;
#else
typedef int intptr_t;
#endif

#ifdef PNUT_SH
// on pnut-sh, the file can only be opened in 3 modes: read, write and append
// if it doesn't exist, it will be created.
#define O_WRONLY 01
#define O_CREAT  00
#define O_TRUNC  00
#else
#define O_WRONLY 01
#define O_CREAT  0100
#define O_TRUNC  01000
#endif
#endif

// =============================================================================

#define ast int
#define true 1
#define false 0
#define EOF (-1)

typedef int bool;

#ifdef PNUT_CC

typedef int FILE;

#endif

// State for the reader
//  - fp: current file pointer
//  - fp_filepath: path of the current file being read, used for error messages.
//  - fp_dirname: directory of the current file, used to resolve relative paths.
//  - include_search_path: search path for system include files.
//  - output_fd: the output file descriptor (1 = stdout), used by pnut-exe
//  - line_number: line number of the current file, used for error messages.
//  - column_number: column number of the current file, used for error messages.
//  - last_tok_line_number: line number of the last token read, used for error messages.
//  - last_tok_column_number: column number of the last token read, used for error messages.
//  - include_stack: the stack to save the state of the reader when including a file.

FILE *fp = 0; // Current file pointer that's being read
char* fp_filepath = 0; // The path of the current file being read
char *fp_dirname = 0; // The directory of the current file being read
char* include_search_path = 0; // Search path for include files
int output_fd = 1; // Output file descriptor (1 = stdout)

#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
int line_number = 1;
int column_number = 0;
int last_tok_line_number = 1;
int last_tok_column_number = 0;
#define INCLUDE_ENTRY_SIZE 5
#else
#define INCLUDE_ENTRY_SIZE 3
#endif

#define INCLUDE_STACK_DEPTH 10

intptr_t include_stack[INCLUDE_STACK_DEPTH * INCLUDE_ENTRY_SIZE];
int include_stack_top = 0; // Point to top of the stack, i.e. the next free entry

void putstr(char *str) {
  while (*str) {
    putchar(*str);
    str += 1;
  }
}

#ifdef PNUT_SH
#define putint(n) printf("%d", n)
#define puthex_unsigned(n) printf("%x", n)
#define putoct_unsigned(n) printf("%o", n)
#else
void putint_aux(int n) {
  if (n <= -10) putint_aux(n / 10);
  putchar('0' - (n % 10));
}

// Output signed integer
void putint(int n) {
  if (n < 0) {
    putchar('-');
    putint_aux(n);
  } else {
    putint_aux(-n);
  }
}

#ifdef sh

// Output unsigned integer in hex
void puthex_unsigned(int n) {
  // Because n is signed, we clear the upper bits after shifting in case n was negative
  if ((n >> 4) & 0x0fffffff) puthex_unsigned((n >> 4) & 0x0fffffff);
  putchar("0123456789abcdef"[n & 15]);
}

// Output unsigned integer in octal
void putoct_unsigned(int n) {
  // Because n is signed, we clear the upper bits after shifting in case n was negative
  if ((n >> 3) & 0x1fffffff) putoct_unsigned((n >> 3) & 0x1fffffff);
  putchar('0' + (n & 7));
}
#endif

#endif

#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
void print_tok_type(int tok); // Imported from debug.c later in this file

void stop_compiler(char *error_prefix, char *error_msg, bool show_token_info, int token) {
#ifdef NICE_ERR_MSG
  #define ANSI_RED     "\x1b[31m"
  #define ANSI_GREEN   "\x1b[32m"
  #define ANSI_YELLOW  "\x1b[33m"
  #define ANSI_BLUE    "\x1b[34m"
  #define ANSI_MAGENTA "\x1b[35m"
  #define ANSI_CYAN    "\x1b[36m"
  #define ANSI_RESET   "\x1b[0m"

  // Error header
  printf(ANSI_RED"%s", error_prefix);
  printf(ANSI_GREEN"%s\n"ANSI_RESET, fp_filepath);
  printf("  Reason: "ANSI_YELLOW"%s"ANSI_RESET"\n", error_msg);
  if (show_token_info) {
    printf("  Offending token: "ANSI_YELLOW);
    print_tok_type(token);
    printf(ANSI_RESET"\n");
  }
  if (fp_filepath != 0) {
    printf("  Location: "ANSI_GREEN"%s:%d:%d"ANSI_RESET"\n", fp_filepath, last_tok_line_number, last_tok_column_number);
  }
#else
  if (fp_filepath != 0) {
    printf("%s:%d:%d: ", fp_filepath, last_tok_line_number, last_tok_column_number);
  }
  printf("%s%s\n", error_prefix, error_msg);
#endif
  exit(1);
}

#else

#define stop_compiler(error_prefix, error_msg, show_token_info, tok) \
  putstr(error_prefix); putstr(error_msg); putstr("\n"); exit(1);

#endif

void fatal_error(char * const msg) {
  stop_compiler("Fatal error: ", msg, false, 0);
}

void syntax_error(char * const msg) {
  stop_compiler("Syntax error: ", msg, false, 0);
}

void parse_error(char * const msg, const int tok) {
  stop_compiler("Parse error: ", msg, true, tok);
}

// Before including a file, we save the state of the reader to the include stack.
// This allows us to restore the previous file pointer and file name when we finish including the file.
void save_include_context() {
  if (include_stack_top >= INCLUDE_STACK_DEPTH * INCLUDE_ENTRY_SIZE) fatal_error("Include stack overflow");

  if (fp != 0) {
    include_stack[include_stack_top]     = (intptr_t)fp;
    include_stack[include_stack_top + 1] = (intptr_t)fp_filepath;
    include_stack[include_stack_top + 2] = (intptr_t)fp_dirname;

  #ifdef INCLUDE_LINE_NUMBER_ON_ERROR
    // Save the line and column number of the current file
    include_stack[include_stack_top + 3] = line_number;
    include_stack[include_stack_top + 4] = column_number;
  #endif
    include_stack_top += INCLUDE_ENTRY_SIZE;
  }
}

void restore_include_context() {
  if (include_stack_top == 0) fatal_error("Include stack is empty");

  fclose(fp);
  if (fp_dirname != 0) free(fp_dirname);
  // We skip freeing the filepath because it may belong to the string pool

  include_stack_top -= INCLUDE_ENTRY_SIZE;
  // Must add parentheses because M2-Planet parses the cast operator with higher
  // precedence than the dereference operator.
  fp          = (FILE*) (include_stack[include_stack_top]);
  fp_filepath = (char*) (include_stack[include_stack_top + 1]);
  fp_dirname  = (char*) (include_stack[include_stack_top + 2]);
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  line_number   = include_stack[include_stack_top + 3];
  column_number = include_stack[include_stack_top + 4];
#endif
}

// Tokens and AST nodes
enum {
  // C keywords
  KEYWORDS_START = 300,
  BREAK_KW,
  CASE_KW,
  CONTINUE_KW,
  DEFAULT_KW,
#ifdef SUPPORT_DO_WHILE
  DO_KW,
#endif
  ELSE_KW,
  FOR_KW,
  IF_KW,
  RETURN_KW,
#ifdef SUPPORT_SIZEOF
  SIZEOF_KW,
#endif
  SWITCH_KW,
  TYPEDEF_KW,
  WHILE_KW,

// Type qualifiers and storage class specifiers
  CONST_KW,
#ifdef SUPPORT_TYPE_SPECIFIERS
  AUTO_KW,
  EXTERN_KW,
  REGISTER_KW,
  STATIC_KW,
  VOLATILE_KW,
  INLINE_KW,
#endif

  // Type specifiers
  // Must be below storage class specifiers for MK_TYPE_SPECIFIER to work correctly
  CHAR_KW,
  DOUBLE_KW,
  ENUM_KW,
  FLOAT_KW,
  INT_KW,
  LONG_KW,
  SHORT_KW,
  SIGNED_KW,
  UNSIGNED_KW,
  VOID_KW,

#ifdef SUPPORT_GOTO
  GOTO_KW,
#endif
#ifdef SUPPORT_STRUCT_UNION
  STRUCT_KW,
  UNION_KW,
#endif
  KEYWORDS_END,

  // Non-character operands
  INTEGER     = 401, // Integer written in decimal
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
  INTEGER_HEX = 402, // Integer written in hexadecimal
  INTEGER_OCT = 403, // Integer written in octal
#endif
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
  INTEGER_L   = 404,
  INTEGER_LL,
  INTEGER_U,
  INTEGER_UL,
  INTEGER_ULL,
#endif
  CHARACTER = 410, // Fixed value so the ifdef above don't change the value
  STRING    = 411,

  AMP_AMP   = 450,
  AMP_EQ,
#ifdef SUPPORT_STRUCT_UNION
  ARROW,
#endif
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
#ifdef FULL_PREPROCESSOR_SUPPORT
  HASH_HASH,
#endif
  PLUS_PLUS_PRE,
  MINUS_MINUS_PRE,
  PLUS_PLUS_POST,
  MINUS_MINUS_POST,
#ifdef SUPPORT_VARIADIC_FUNCTIONS
  ELLIPSIS,
#endif
#ifdef SUPPORT_COMPLEX_INITIALIZER
  INITIALIZER_LIST,
#endif
  DECL,
  DECLS,
  FUN_DECL,
  CAST,
  MACRO_ARG = 499,
  IDENTIFIER = 500, // 500 because it's easy to remember
  TYPE = 501,
  MACRO = 502,

  LIST = 600, // List object
};

// tokenizer

int ch;
#ifdef DEBUG_EXPAND_INCLUDES
int prev_ch = EOF;
#endif
int tok;
int val;

#define STRING_POOL_SIZE 250000
char string_pool[STRING_POOL_SIZE];
int string_pool_alloc = 0;
int string_start;
int hash;

// These parameters give a perfect hashing of the C keywords
#define HASH_PARAM 1026
#define HASH_PRIME 1009
// Some C implementations place globals on the stack, where size is limited.
#ifdef SMALL_HEAP
#define HEAP_SIZE 131072 // 128 KB
#else
#define HEAP_SIZE 786432 // 768 KB
#endif
intptr_t heap[HEAP_SIZE];
int heap_alloc = HASH_PRIME;

int alloc_result;

int alloc_obj(const int size) {

  alloc_result = heap_alloc;

  heap_alloc += size;

  if (heap_alloc > HEAP_SIZE) {
    fatal_error("heap overflow");
  }

  return alloc_result;
}

int get_op(const ast node) {
  return heap[node] & 1023;
}

ast get_nb_children(const ast node) {
  return heap[node] >> 10;
}

// Because everything is an int in pnut, it's easy to make mistakes and pass the
// wrong node type to a function. These versions of get_child take the input
// and/or output node type and checks that the node has the expected type before
// returning the child node.
// It also checks that the index is within bounds.
#ifdef SAFE_MODE
int get_val_checked(char* file, int line, ast node) {
  if (get_nb_children(node) != 0) {
    printf("%s:%d: get_val called on node %d with %d children\n", file, line, get_op(node), get_nb_children(node));
    exit(1);
  }
  return heap[node+1];
}

int get_val_go(char* file, int line, int expected_node, ast node) {
  if (get_op(node) != expected_node) {
    printf("%s:%d: Expected node %d, got %d\n", file, line, expected_node, get_op(node));
    exit(1);
  }
  return get_val_checked(file, line, node);
}

void set_val_checked(char* file, int line, ast node, int val) {
  if (get_nb_children(node) != 0) {
    printf("%s:%d: set_val called on node %d with %d children\n", file, line, get_op(node), get_nb_children(node));
    exit(1);
  }
  heap[node+1] = val;
}

ast get_child_checked(char* file, int line, ast node, int i) {
  if (i != 0 && i >= get_nb_children(node)) {
    printf("%s:%d: Index %d out of bounds for node %d\n", file, line, i, get_op(node));
    exit(1);
  }
  return heap[node+i+1];
}

void set_child_checked(char* file, int line, ast node, int i, ast child) {
  if (i != 0 && i >= get_nb_children(node)) {
    printf("%s:%d: Index %d out of bounds for node %d\n", file, line, i, get_op(node));
    exit(1);
  }
  heap[node+i+1] = child;
}

// This function checks that the parent node has the expected operator before
// returning the child node.
ast get_child_go(char* file, int line, int expected_parent_node, ast node, int i) {
  ast res = get_child_checked(file, line, node, i);
  if (get_op(node) != expected_parent_node) {
    printf("%s:%d: Expected node %d, got %d\n", file, line, expected_parent_node, get_op(node));
    exit(1);
  }
  return res;
}

// This function checks that the parent node has the expected operator and that
// the child node has the expected operator before returning the child node.
ast get_child__go(char* file, int line, int expected_parent_node, int expected_node, ast node, int i) {
  ast res = get_child_checked(file, line, node, i);
  if (get_op(node) != expected_parent_node) {
    printf("%s:%d: Expected node %d, got %d\n", file, line, expected_parent_node, get_op(node));
    exit(1);
  }
  if (get_op(res) != expected_node) {
    printf("%s:%d: Expected child node %d, got %d\n", file, line, expected_node, get_op(res));
    exit(1);
  }
  return res;
}

// This function checks that the parent node has the expected operator and that
// the child node has the expected operator (if child node is not 0) before
// returning the child node.
ast get_child_opt_go(char* file, int line, int expected_parent_node, int expected_node, ast node, int i) {
  ast res = get_child_checked(file, line, node, i);
  if (get_op(node) != expected_parent_node) {
    printf("%s:%d: Expected node %d, got %d\n", file, line, expected_parent_node, get_op(node));
    exit(1);
  }
  if (res > 0 && get_op(res) != expected_node) {
    printf("%s:%d: Expected child node %d, got %d\n", file, line, expected_node, get_op(res));
    exit(1);
  }
  return res;
}

#define get_val(node) get_val_checked(__FILE__, __LINE__, node)
#define get_val_(expected_node, node) get_val_go(__FILE__, __LINE__, expected_node, node)
#define set_val(node, val) set_val_checked(__FILE__, __LINE__, node, val)
#define set_child(node, i, child) set_child_checked(__FILE__, __LINE__, node, i, child)
#define get_child(node, i) get_child_checked(__FILE__, __LINE__, node, i)
#define get_child_(expected_parent_node, node, i) get_child_go(__FILE__, __LINE__, expected_parent_node, node, i)
#define get_child__(expected_parent_node, expected_node, node, i) get_child__go(__FILE__, __LINE__, expected_parent_node, expected_node, node, i)
#define get_child_opt_(expected_parent_node, expected_node, node, i) get_child_opt_go(__FILE__, __LINE__, expected_parent_node, expected_node, node, i)

#else

int get_val(const ast node) {
  return heap[node+1];
}

void set_val(const ast node, const int val) {
  heap[node+1] = val;
}

ast get_child(const ast node, const int i) {
  return heap[node+i+1];
}

void set_child(const ast node, const int i, const ast child) {
  heap[node+i+1] = child;
}

#define get_val_(expected_node, node) get_val(node)
#define get_child_(expected_parent_node, node, i) get_child(node, i)
#define get_child__(expected_parent_node, expected_node, node, i) get_child(node, i)
#define get_child_opt_(expected_parent_node, expected_node, node, i) get_child(node, i)

#endif

ast ast_result;

ast new_ast0(const int op, const int val) {

  ast_result = alloc_obj(2);

  heap[ast_result] = op;
  set_val(ast_result, val);

  return ast_result;
}

ast new_ast1(const int op, const ast child0) {

  ast_result = alloc_obj(2);

  heap[ast_result] = op + 1024;
  set_child(ast_result, 0, child0);

  return ast_result;
}

ast new_ast2(const int op, const ast child0, const ast child1) {

  ast_result = alloc_obj(3);

  heap[ast_result] = op + 2048;
  set_child(ast_result, 0, child0);
  set_child(ast_result, 1, child1);

  return ast_result;
}

ast new_ast3(const int op, const ast child0, const ast child1, const ast child2) {

  ast_result = alloc_obj(4);

  heap[ast_result] = op + 3072;
  set_child(ast_result, 0, child0);
  set_child(ast_result, 1, child1);
  set_child(ast_result, 2, child2);

  return ast_result;
}

ast new_ast4(const int op, const ast child0, const ast child1, const ast child2, const ast child3) {

  ast_result = alloc_obj(5);

  heap[ast_result] = op + 4096;
  set_child(ast_result, 0, child0);
  set_child(ast_result, 1, child1);
  set_child(ast_result, 2, child2);
  set_child(ast_result, 3, child3);

  return ast_result;
}

ast clone_ast(const ast orig) {
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

ast cons(const int child0, const int child1)    { return new_ast2(LIST, child0, child1); }
ast car(const int pair)                         { return get_child_(LIST, pair, 0); }
ast cdr(const int pair)                         { return get_child_(LIST, pair, 1); }
#ifdef SAFE_MODE
ast car_(const int expected_op, const int pair) { return get_child__(LIST, expected_op, pair, 0); }
ast cdr_(const int expected_op, const int pair) { return get_child_opt_(LIST, expected_op, pair, 1); }
#else
#define car_(expected_op, pair)                 car(pair)
#define cdr_(expected_op, pair)                 cdr(pair)
#endif
// void set_car(const int pair, const int value)   { return set_child(pair, 0, value); }
void set_cdr(const int pair, const int value)   { return set_child(pair, 1, value); }
#ifndef sh
ast list1(const int child0)                     { return new_ast2(LIST, child0, 0); }
ast list2(const int child0, const int child1)   { return new_ast2(LIST, child0, new_ast2(LIST, child1, 0)); }
ast list3(const int child0, const int child1, const int child2) { return new_ast2(LIST, child0, new_ast2(LIST, child1, new_ast2(LIST, child2, 0))); }
#endif
#define tail(x) cdr_(LIST, x)

#ifdef sh
// Returns the only element of a singleton list, if it is a singleton list.
// Otherwise, returns 0.
ast list_singleton(const ast list) {
  if (list != 0 && tail(list) == 0) {
    return car(list);
  } else {
    return 0;
  }
}
#endif

// Simple accessor to get the string from the string pool
#define STRING_BUF(probe) (string_pool + heap[probe+1])
#define STRING_LEN(probe) (heap[probe+4])
#define STRING_BUF_END(probe) (STRING_BUF(probe) + STRING_LEN(probe))

void begin_string() {
  string_start = string_pool_alloc;
  hash = 0;
}

// Append the current character (ch) to the string under construction in the pool
void accum_string(const char c) {
  hash = (c + (hash ^ HASH_PARAM)) % HASH_PRIME;
  string_pool[string_pool_alloc] = c;
  string_pool_alloc += 1;
  if (string_pool_alloc >= STRING_POOL_SIZE) {
    fatal_error("string pool overflow");
  }
}

// Append a string from the string_pool to the string under construction
void accum_string_string(const int string_probe) {
  char *string_start = STRING_BUF(string_probe);
  char *string_end = string_start + STRING_LEN(string_probe);
  while (string_start < string_end) {
    accum_string(*string_start);
    string_start += 1;
  }
}

#ifdef FULL_PREPROCESSOR_SUPPORT

// Similar to accum_string_string, but writes an integer to the string pool
// Note that this function only supports small integers, represented as positive number.
void accum_string_integer(int n) {
#ifdef SUPPORT_64_BIT_LITERALS
  if (n < 0) fatal_error("accum_string_integer: Only small integers can be pasted");
#else
  if (n < 0) {
    accum_string('-');
    accum_string_integer(-n);
  } else
#endif
  {
    if (n > 9) accum_string_integer(n / 10);
    accum_string('0' + n % 10);
  }
}

#endif

int probe;
int probe_start;
char c1;
char c2;
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
  heap[probe+4] = string_pool_alloc - string_start - 1; // string length (excluding terminator)

  return probe;
}

#ifdef SAFE_MODE

// Debugging functions
// When bootstrapping, the debugging functions are disabled to reduce code size.

void dump_string(char *prefix, char *str) {
  putstr(prefix);
  putchar('"');
  putstr(str);
  putchar('"');
  putchar('\n');
}

void dump_int(char *prefix, int n) {
  putstr(prefix);
  putint(n);
  putchar('\n');
}

void dump_char(int c) {
  dump_int("char = ", c);
}

void dump_tok(int tok) {
  dump_int("tok = ", tok);
}

void dump_ident(int probe) {
  dump_string("ident = ", STRING_BUF(probe));
}

void dump_node(ast node) {
  putstr("op=");
  putint(get_op(node));
  putstr(" with #children =");
  putint(get_nb_children(node));
  putchar('\n');
}

void dump_op(int op) {
  dump_int("op = ", op);
}

#else
#define dump_string(prefix, str)
#define dump_int(prefix, n)
#define dump_char(c)
#define dump_tok(tok)
#define dump_ident(probe)
#define dump_node(node)
#define dump_op(op)
#endif

#define IFDEF_DEPTH_MAX 20
bool if_macro_stack[IFDEF_DEPTH_MAX]; // Stack of if macro states
bool if_macro_stack_ix = 0;
bool if_macro_mask = true;      // Indicates if the current if/elif block is being executed
bool if_macro_executed = false; // If any of the previous if/elif conditions were true

// get_tok parameters:
// Whether to expand macros or not.
// Useful to parse macro definitions containing other macros without expanding them.
bool expand_macro = true;
// Don't expand macro arguments. Used for stringification and token pasting.
bool expand_macro_arg = true;
// Don't produce newline tokens. Used when reading the tokens of a macro definition.
bool skip_newlines = true;

#define MACRO_RECURSION_MAX 180 // Supports up to 60 (180 / 3) nested macro expansions.
int macro_stack[MACRO_RECURSION_MAX];
int macro_stack_ix = 0;

int macro_tok_lst = 0;  // Current list of tokens to replay for the macro being expanded
int macro_args = 0;     // Current list of arguments for the macro being expanded
int macro_ident = 0;    // The identifier of the macro being expanded (if any)
int macro_args_count;   // Number of arguments for the current macro being expanded
bool paste_last_token = false; // Whether the last token was a ## or not

bool prev_macro_mask() {
  return if_macro_stack_ix == 0 || if_macro_stack[if_macro_stack_ix - 2];
}

void push_if_macro_mask(bool new_mask) {
  if (if_macro_stack_ix >= IFDEF_DEPTH_MAX) {
    fatal_error("Too many nested #ifdef/#ifndef directives. Maximum supported is 20.");
  }
  // Save current mask on the stack because it's about to be overwritten
  if_macro_stack[if_macro_stack_ix] = if_macro_mask;
  if_macro_stack[if_macro_stack_ix + 1] = if_macro_executed;
  if_macro_stack_ix += 2;

  // If the current block is masked off, then the new mask is the logical AND of the current mask and the new mask
  new_mask = if_macro_mask & new_mask;

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
#define C_CODE_BUF_LEN 20000

char code_char_buf[C_CODE_BUF_LEN];
int code_char_buf_ix = 0;
// Point to the **last** character of the **last** token.
// This is used to skip the current token when printing the code of a
// declaration since it belongs to the next declaration.
int last_tok_code_buf_ix = 0;

void output_declaration_c_code(bool no_header) {

  int i = 0;

  if (!no_header) {
    putstr("#################################### C code ####################################\n");
  }
  putchar('#');
  putchar(' ');

  // Skip leading newlines if any.
  while (code_char_buf[i] == '\n') i += 1;

  for (; i < last_tok_code_buf_ix; i += 1) {

    if (code_char_buf[i] == '\n') {
      // Condense the C code by removing extra newlines
      if (code_char_buf[i - 1] != code_char_buf[i]) {
        putchar('\n');
        putchar('#');
        putchar(' ');
      }
    } else {
      putchar(code_char_buf[i]);
    }
  }

  // End of decl
  putchar('\n');
  if (!no_header) {
    putstr("################################# End of C code ################################\n");
  }

  // Copy the last token characters to the beginning of the buffer
  for (i = 0; i < code_char_buf_ix - last_tok_code_buf_ix; i += 1) {
    code_char_buf[i] = code_char_buf[last_tok_code_buf_ix + i];
  }

  code_char_buf_ix = i;
}
#endif

#ifdef SUPPORT_LINE_CONTINUATION
// get_ch_ is reponsible for reading the next character from the input file,
// switching to the next file if necessary and updating the line number.
// get_ch is then responsible for skipping line continuations.
void get_ch_();

int line_continutation_prev_char = -2; // -1 is EOF, -2 is uninitialized
void get_ch() {
  if (line_continutation_prev_char == -2) {
    while (1) {      // Loop as long as we're reading line continuations
      get_ch_();     // Read the next character
      if (ch == '\\') {
        get_ch_();   // Skip backslash
        if (ch == '\n') {
          continue; // Loop again to read the next character
        } else {
          // '\' is not followed by newline, so we save the current character
          // and make '\' the current character
          line_continutation_prev_char = ch;
          ch = '\\';
          break;
        }
      } else {
        break;
      }
    }
  } else {
    ch = line_continutation_prev_char;
    line_continutation_prev_char = -2;
  }
}

void get_ch_() {
#else
void get_ch() {
#endif
  ch = fgetc(fp);

  if (ch == EOF) {
    // If it's not the last file on the stack, EOF means that we need to switch to the next file
    if (include_stack_top != 0) {
      restore_include_context();
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
  code_char_buf[code_char_buf_ix] = ch;
  code_char_buf_ix += 1;
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
    return 0;
  } else {
    return substr(path, 0, last_slash + 1);
  }
}

void include_file(char *file_name, char *relative_to) {
  save_include_context();
  fp_filepath = file_name;
  if (relative_to) {
    fp_filepath = str_concat(relative_to, fp_filepath);
  }
  fp = fopen(fp_filepath, "r");
  if (fp == 0) {
    dump_string("#include ", fp_filepath);
    fatal_error("Could not open file");
  }

  fp_dirname = file_parent_directory(fp_filepath);
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  line_number = 1;
  column_number = 0;
#endif
}

#ifdef SUPPORT_64_BIT_LITERALS
// Array used to accumulate 64 bit unsigned integers on 32 bit systems
int val_32[2];

// x = x * y
void u64_mul_u32(int *x, int y) {

  // Note, because we are using 32 bit **signed** integers, we need to clear the
  // sign bit when shifting right to avoid sign extension.
  #define I32_LOGICAL_RSHIFT_16(x) ((x >> 16) & 0xffff)

  int xlo = x[0] & 0xffff;
  int xhi = I32_LOGICAL_RSHIFT_16(x[0]);
  int ylo = y & 0xffff;
  int yhi = I32_LOGICAL_RSHIFT_16(y);
  int lo = xlo * ylo; /* 0 .. 0xfffe0001 */
  int m1 = xlo * yhi + (lo >> 16); /* 0 .. 0xfffeffff */
  int m2 = xhi * ylo; /* 0 .. 0xfffe0001 */
  int m3 = (m1 & 0xffff) + (m2 & 0xffff); /* 0 .. 0x1fffe */
  int hi = xhi * yhi + I32_LOGICAL_RSHIFT_16(m1) + I32_LOGICAL_RSHIFT_16(m2) + I32_LOGICAL_RSHIFT_16(m3); /* 0 .. 0xfffffffe */
  x[0] = ((m3 & 0xffff) << 16) + (lo & 0xffff);
  x[1] = x[1] * y + hi;
}

// x = x + y
void u64_add_u32(int *x, int y) {
  int lo = x[0] + y;
  // Carry (using signed integers)
  x[1] += ((x[0] < 0) != (lo < 0));
  x[0] = lo;
}

// Pack a 64 bit unsigned integer into an object.
// Because most integers are small and we want to save memory, we only store the
// large int object ("large ints") if it is larger than 31 bits. Otherwise, we
// store it as a regular integer. The sign bit is used to distinguish between
// large ints (positive) and regular ints (negative).
void u64_to_obj(int *x) {
  if (x[0] >= 0 && x[1] == 0) { // "small int"
    val = -x[0];
  } else {
    val = alloc_obj(2);
    heap[val    ] = x[0];
    heap[val + 1] = x[1];
  }
}

#define DIGIT_BYTE (val_32[0] % 256)
#define INIT_ACCUM_DIGIT() val_32[0] = 0; val_32[1] = 0;
#else
#define DIGIT_BYTE (-val % 256)
#define INIT_ACCUM_DIGIT() val = 0;
#endif

int accum_digit(int base) {
  int digit = 99;
  int MININT = -2147483648;
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
    int limit = MININT / base;
    if (base == 10 && if_macro_mask && ((val < limit) || ((val == limit) && (digit > limit * base - MININT)))) {
      fatal_error("literal integer overflow");
    }

#ifdef SUPPORT_64_BIT_LITERALS
    u64_mul_u32(val_32, base);
    u64_add_u32(val_32, digit);
#else
    val = val * base - digit;
#endif
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
      INIT_ACCUM_DIGIT();
      accum_digit(8);
      accum_digit(8);
      accum_digit(8);
      val = DIGIT_BYTE; // keep low 8 bits, without overflowing
    } else if (ch == 'x' || ch == 'X') {
      get_ch();
      INIT_ACCUM_DIGIT();
      // Allow 1 or 2 hex digits.
      if (accum_digit(16)) {
        accum_digit(16);
      } else {
        syntax_error("invalid hex escape -- it must have at least one digit");
      }
      val = DIGIT_BYTE; // keep low 8 bits, without overflowing
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
    accum_string(val);
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

#ifdef FULL_PREPROCESSOR_SUPPORT
int NOT_SUPPORTED_ID;
#endif

// We want to recognize certain identifers without having to do expensive string comparisons
int MAIN_ID;
#ifdef sh
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

// In zsh, writing to argv assigns to $@, so we map argv to argv_, and forbid
// argv_ since argv is a common C variable name.
int ARGV__ID;
int ARGV_ID;
// Shell special variables
int ENV_ID;
int HOME_ID;
int IFS_ID;
int LANG_ID;
int LC_ALL_ID;
int LC_COLLATE_ID;
int LC_CTYPE_ID;
int LC_MESSAGES_ID;
int LINENO_ID;
int NLSPATH_ID;
int PATH_ID;
int PPID_ID;
int PS1_ID;
int PS4_ID;
int PWD_ID;
#endif

#ifdef FULL_PREPROCESSOR_SUPPORT
// Macros that are defined by the preprocessor
int FILE__ID;
int LINE__ID;
#endif

void get_tok();

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
  int rest;

  // Accumulate tokens so they can be replayed when the macro is used
  if (tok != '\n' && tok != EOF) {
    // Append the token/value pair to the replay list
    toks = cons(lookup_macro_token(args, tok, val), 0);
    rest = toks;
    get_tok_macro();
    while (tok != '\n' && tok != EOF) {
      set_cdr(rest, cons(lookup_macro_token(args, tok, val), 0));
      rest = cdr(rest); // Advance tail
      get_tok_macro();
    }

#ifdef FULL_PREPROCESSOR_SUPPORT
    // Check that there are no leading or trailing ##
    if (car(car(toks)) == HASH_HASH || car(car(rest)) == HASH_HASH) {
      syntax_error("'##' cannot appear at either end of a macro expansion");
    }
#endif
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

  if (tok != IDENTIFIER && tok != MACRO && (tok < KEYWORDS_START || tok > KEYWORDS_END)) {
    dump_tok(tok);
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

  int val0, val1; // Results of evaluating child0 and child1, for non-lazy operators
  if (op != '?' && op != AMP_AMP && op != BAR_BAR && op != '(') {
    // For non-lazy operators, we can pre-evaluate the children.
    // This makes eval_constant's shell version much shorter and easier to review.
    if (get_nb_children(expr) >= 1) val0 = eval_constant(get_child(expr, 0), if_macro);
    if (get_nb_children(expr) >= 2) val1 = eval_constant(get_child(expr, 1), if_macro);
  }

  switch (op) {
    case INTEGER:
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
    case INTEGER_L:
    case INTEGER_LL:
    case INTEGER_U:
    case INTEGER_UL:
    case INTEGER_ULL:
#endif
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
    case INTEGER_HEX:
    case INTEGER_OCT:
#endif
#ifdef SUPPORT_64_BIT_LITERALS
      // Disable large integers for now, hopefully they don't appear in TCC in enums and #if expressions
      if (get_val(expr) > 0) fatal_error("constant expression too large");
#endif
      return -get_val(expr);
    case CHARACTER:   return get_val_(CHARACTER, expr);
    case '~':         return ~val0;
    case '!':         return !val0;
    case '*':         return val0 *  val1;
    case '/':         return val0 /  val1;
    case '%':         return val0 %  val1;
    case '&':         return val0 &  val1;
    case '|':         return val0 |  val1;
    case '^':         return val0 ^  val1;
    case LSHIFT:      return val0 << val1;
    case RSHIFT:      return val0 >> val1;
    case EQ_EQ:       return val0 == val1;
    case EXCL_EQ:     return val0 != val1;
    case LT_EQ:       return val0 <= val1;
    case GT_EQ:       return val0 >= val1;
    case '<':         return val0 <  val1;
    case '>':         return val0 >  val1;

    // - and + can be unary or binary
    case '-':
    case '+':
      if (get_nb_children(expr) == 1) {
        return TERNARY(op == '-', -val0, val0);
      } else {
        return TERNARY(op == '-', (val0 - val1), (val0 + val1));
      }

    case '?':
      val0 = eval_constant(get_child(expr, 0), if_macro);
      if (val0) {
        return eval_constant(get_child(expr, 1), if_macro);
      } else {
        return eval_constant(get_child(expr, 2), if_macro);
      }

    case AMP_AMP:
    case BAR_BAR:
      val0 = eval_constant(get_child(expr, 0), if_macro);
      if (op == AMP_AMP && !val0) return 0;
      else if (op == BAR_BAR && val0) return 1;
      else return eval_constant(get_child(expr, 1), if_macro);

    case '(': // defined operators are represented as fun calls
      if (if_macro && get_val_(IDENTIFIER, get_child(expr, 0)) == DEFINED_ID) {
        return get_child(expr, 1) == MACRO;
      } else {
        fatal_error("unknown function call in constant expressions");
        return 0;
      }

    case IDENTIFIER:
      if (!if_macro) {
        // At this point, macros have already been expanded so we can't have a
        // macro identifier, which means we're dealing with a regular identifier.
        // TODO: Enums when outside of if_macro
        fatal_error("identifiers are not allowed in constant expression");
      }

      return 0; // Undefined identifiers evaluate to 0

    default:
      dump_op(op);
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
    include_file(STRING_BUF(val), fp_dirname);
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
    dump_tok(tok);
    syntax_error("expected string to #include directive");
  }
}

#ifdef SH_SUPPORT_SHELL_INCLUDE
void handle_shell_include();
#endif

void handle_preprocessor_directive() {
  int temp;
#ifdef SH_INCLUDE_C_CODE
  int prev_char_buf_ix = code_char_buf_ix; // Index of the # token in the code buffer
#endif
  get_tok_macro(); // Get the # token
  get_tok_macro(); // Get the directive

  if (tok == IDENTIFIER && (val == IFDEF_ID || val == IFNDEF_ID)) {
    temp = val;
    get_tok_macro(); // Get the macro name
    push_if_macro_mask(TERNARY(temp == IFDEF_ID, tok == MACRO, tok != MACRO));
    get_tok_macro(); // Skip the macro name
  } else if (tok == IF_KW) {
    temp = evaluate_if_condition() != 0;
    push_if_macro_mask(temp);
  } else if (tok == IDENTIFIER && val == ELIF_ID) {
    temp = evaluate_if_condition() != 0;
    if (prev_macro_mask() && !if_macro_executed) {
      if_macro_executed |= temp;
      if_macro_mask = temp;
    } else {
      if_macro_mask = false;
    }
  } else if (tok == ELSE_KW) {
    if (prev_macro_mask()) { // If the parent block mask is true
      if_macro_mask = !if_macro_executed;
      if_macro_executed = true;
    } else {
      if_macro_mask = false;
    }
    get_tok_macro(); // Skip the else keyword
  } else if (tok == IDENTIFIER && val == ENDIF_ID) {
    pop_if_macro_mask();
    get_tok_macro(); // Skip the else keyword
  } else if (if_macro_mask) {
    if (tok == IDENTIFIER && val == INCLUDE_ID) {
      get_tok_macro(); // Get the STRING token
      handle_include();
    }
#ifdef SH_SUPPORT_SHELL_INCLUDE
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
        dump_tok(tok);
        syntax_error("#undef directive can only be followed by a identifier");
      }
    } else if (tok == IDENTIFIER && val == DEFINE_ID) {
      get_tok_macro(); // Get the macro name
      handle_define();
    } else if (tok == IDENTIFIER && (val == WARNING_ID || val == ERROR_ID)) {
#ifndef DEBUG_EXPAND_INCLUDES
      temp = val;
      putstr(TERNARY(temp == WARNING_ID, "warning: ", "error: "));
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
      dump_tok(tok);
      dump_string("directive = ", STRING_BUF(val));
      syntax_error("unsupported preprocessor directive");
    }
  } else {
    // Skip the rest of the directive
    while (tok != '\n' && tok != EOF) get_tok_macro();
  }

  if (tok != '\n' && tok != EOF) {
    dump_tok(tok);
    if (tok == IDENTIFIER || tok == MACRO) {
      dump_string("directive = ", STRING_BUF(val));
    }
    syntax_error("preprocessor expected end of line");
  }

  // Because handle_preprocessor_directive is called from get_tok, and it loops
  // after the call to handle_preprocessor_directive, we don't need to call
  // get_tok before returning.

#ifdef SH_INCLUDE_C_CODE
  code_char_buf_ix = prev_char_buf_ix - 1;
  // Copy the current char and a newline, because they were consumed by the last get_tok call
  code_char_buf[code_char_buf_ix++] = '\n';
  code_char_buf[code_char_buf_ix++] = ch;
#endif
}

void get_ident() {

  begin_string();

  while (('A' <= ch && ch <= 'Z') ||
         ('a' <= ch && ch <= 'z') ||
         ('0' <= ch && ch <= '9') ||
         (ch == '_')) {
    accum_string(ch);
    get_ch();
  }

  val = end_ident();
  tok = heap[val+2];
}

int intern_str(char* name) {
  int i = 0;

  begin_string();

  while (name[i] != 0) {
    accum_string(name[i]);
    i += 1;
  }

  i = end_string();

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

  init_ident(BREAK_KW,    "break");
  init_ident(CASE_KW,     "case");
  init_ident(CONTINUE_KW, "continue");
  init_ident(DEFAULT_KW,  "default");
#ifdef SUPPORT_DO_WHILE
  init_ident(DO_KW,       "do");
#endif
  init_ident(ELSE_KW,     "else");
  init_ident(FOR_KW,      "for");
  init_ident(IF_KW,       "if");
  init_ident(RETURN_KW,   "return");
#ifdef SUPPORT_SIZEOF
  init_ident(SIZEOF_KW,   "sizeof");
#endif
  init_ident(SWITCH_KW,   "switch");
  init_ident(TYPEDEF_KW,  "typedef");
  init_ident(WHILE_KW,    "while");

  // Type specifiers
  init_ident(CHAR_KW,     "char");
  init_ident(DOUBLE_KW,   "double");
  init_ident(ENUM_KW,     "enum");
  init_ident(FLOAT_KW,    "float");
  init_ident(INT_KW,      "int");
  init_ident(LONG_KW,     "long");
  init_ident(SHORT_KW,    "short");
  init_ident(SIGNED_KW,   "signed");
  init_ident(UNSIGNED_KW, "unsigned");
  init_ident(VOID_KW,     "void");

// Type qualifiers and storage class specifiers
  init_ident(CONST_KW,    "const");
#ifdef SUPPORT_TYPE_SPECIFIERS
  init_ident(AUTO_KW,     "auto");
  init_ident(EXTERN_KW,   "extern");
  init_ident(REGISTER_KW, "register");
  init_ident(STATIC_KW,   "static");
  init_ident(VOLATILE_KW, "volatile");
  init_ident(INLINE_KW,   "inline");
#endif

#ifdef SUPPORT_GOTO
  init_ident(GOTO_KW,     "goto");
#endif
#ifdef SUPPORT_STRUCT_UNION
  init_ident(STRUCT_KW,   "struct");
  init_ident(UNION_KW,    "union");
#endif

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
#ifdef SH_SUPPORT_SHELL_INCLUDE
  INCLUDE_SHELL_ID = init_ident(IDENTIFIER, "include_shell");
#endif

  MAIN_ID = init_ident(IDENTIFIER, "main");

#ifdef sh
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

  ARGV_ID = init_ident(IDENTIFIER, "argv");
  ARGV__ID = init_ident(IDENTIFIER, "argv_");
  ENV_ID = init_ident(IDENTIFIER, "ENV");
  HOME_ID = init_ident(IDENTIFIER, "HOME");
  IFS_ID = init_ident(IDENTIFIER, "IFS");
  LANG_ID = init_ident(IDENTIFIER, "LANG");
  LC_ALL_ID = init_ident(IDENTIFIER, "LC_ALL");
  LC_COLLATE_ID = init_ident(IDENTIFIER, "LC_COLLATE");
  LC_CTYPE_ID = init_ident(IDENTIFIER, "LC_CTYPE");
  LC_MESSAGES_ID = init_ident(IDENTIFIER, "LC_MESSAGES");
  LINENO_ID = init_ident(IDENTIFIER, "LINENO");
  NLSPATH_ID = init_ident(IDENTIFIER, "NLSPATH");
  PATH_ID = init_ident(IDENTIFIER, "PATH");
  PPID_ID = init_ident(IDENTIFIER, "PPID");
  PS1_ID = init_ident(IDENTIFIER, "PS1");
  PS4_ID = init_ident(IDENTIFIER, "PS4");
  PWD_ID = init_ident(IDENTIFIER, "PWD");
#endif

#ifdef FULL_PREPROCESSOR_SUPPORT
  // Stringizing is recognized by the macro expander, but it returns a hardcoded
  // string instead of the actual value. This may be enough to compile TCC.
  NOT_SUPPORTED_ID = init_ident(IDENTIFIER, "NOT_SUPPORTED");
#endif
}

#if defined(FULL_PREPROCESSOR_SUPPORT) || defined(FULL_CLI_OPTIONS)
int init_builtin_string_macro(char *macro_str, char* value) {
  int macro_id = init_ident(MACRO, macro_str);
  // Macro object shape: ([(tok, val)], arity). -1 arity means it's an object-like macro
  heap[macro_id + 3] = cons(cons(cons(STRING, intern_str(value)), 0), -1);
  return macro_id;
}
#endif

int init_builtin_int_macro(char *macro_str, int value) {
  int macro_id = init_ident(MACRO, macro_str);
  heap[macro_id + 3] = cons(cons(cons(INTEGER, -value), 0), -1);
  return macro_id;
}

#ifdef FULL_CLI_OPTIONS

int init_builtin_empty_macro(char *macro_str) {
  int macro_id = init_ident(MACRO, macro_str);
  heap[macro_id + 3] = cons(0, -1); // -1 means it's an object-like macro, 0 means no tokens
  return macro_id;
}

#endif

void init_pnut_macros() {
  init_builtin_int_macro("PNUT_CC", 1);

#ifdef FULL_PREPROCESSOR_SUPPORT
  init_builtin_string_macro("__DATE__", "Jan  1 1970");
  init_builtin_string_macro("__TIME__", "00:00:00");
  init_builtin_string_macro("__TIMESTAMP__", "Jan  1 1970 00:00:00");
  FILE__ID = init_builtin_string_macro("__FILE__", "<unknown>");
  LINE__ID = init_builtin_int_macro("__LINE__", 0);
#endif

#if defined(sh)
  init_builtin_int_macro("PNUT_SH", 1);
#elif defined(target_i386_linux)
  init_builtin_int_macro("PNUT_EXE", 1);
  init_builtin_int_macro("PNUT_EXE_32", 1);
  init_builtin_int_macro("PNUT_I386", 1);
  init_builtin_int_macro("PNUT_I386_LINUX", 1);
  init_builtin_int_macro("__linux__", 1);
  init_builtin_int_macro("__i386__", 1);
#elif defined (target_x86_64_linux)
  init_builtin_int_macro("PNUT_EXE", 1);
  init_builtin_int_macro("PNUT_EXE_64", 1);
  init_builtin_int_macro("PNUT_X86_64", 1);
  init_builtin_int_macro("PNUT_X86_64_LINUX", 1);
  init_builtin_int_macro("__linux__", 1);
  init_builtin_int_macro("__x86_64__", 1);
#elif defined (target_x86_64_mac)
  init_builtin_int_macro("PNUT_EXE", 1);
  init_builtin_int_macro("PNUT_EXE_64", 1);
  init_builtin_int_macro("PNUT_X86_64", 1);
  init_builtin_int_macro("PNUT_X86_64_MAC", 1);
  init_builtin_int_macro("__x86_64__", 1);
#endif

}

// A macro argument is represented using a list of tokens.
// Macro arguments are split by commas, but commas can also appear in function
// calls and as operators. To distinguish between the two, we need to keep track
// of the parenthesis depth.
int macro_parse_argument() {
  int arg_tokens = 0;
  int parens_depth = 0;
  int rest;

  while ((parens_depth > 0 || (tok != ',' && tok != ')')) && tok != EOF) {
    if (tok == '(') parens_depth += 1; // Enter parenthesis
    if (tok == ')') parens_depth -= 1; // End of parenthesis

    if (arg_tokens == 0) {
      arg_tokens = cons(cons(tok, val), 0);
      rest = arg_tokens;
    } else {
      set_cdr(rest, cons(cons(tok, val), 0));
      rest = cdr(rest);
    }
    get_tok_macro_expand();
  }

  return arg_tokens;
}

void check_macro_arity(int macro_args_count, int macro) {
  int expected_argc = cdr(heap[macro + 3]);
  if (macro_args_count != expected_argc) {
    dump_int("expected_argc = ", expected_argc);
    dump_int("macro_args_count = ", macro_args_count);
    dump_string("macro = ", STRING_BUF(macro));
    syntax_error("macro argument count mismatch");
  }
}

// Reads the arguments of a macro call, where the arguments are split by commas.
// Note that args are accumulated in reverse order, as the macro arguments refer
// to the tokens in reverse order.
int get_macro_args_toks(int macro) {
  int args = 0;
  int macro_args_count = 0;
  bool prev_is_comma = tok == ',';
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

// "Pops" the current macro expansion and restores the previous macro expansion context.
// This is done when the current macro expansion is done.
void return_to_parent_macro() {
  if (macro_stack_ix == 0) fatal_error("return_to_parent_macro: no parent macro");

  macro_stack_ix -= 3;
  macro_tok_lst   = macro_stack[macro_stack_ix];
  macro_args      = macro_stack[macro_stack_ix + 1];
  macro_ident     = macro_stack[macro_stack_ix + 2];
}

// Begins a new macro expansion context, saving the current context onn the macro stack.
// Takes as argument the name of the macro, the tokens to be expanded and the arguments.
void begin_macro_expansion(int ident, int tokens, int args) {
  if (macro_stack_ix + 3 >= MACRO_RECURSION_MAX) {
    fatal_error("Macro recursion depth exceeded.");
  }

  macro_stack[macro_stack_ix]     = macro_tok_lst;
  macro_stack[macro_stack_ix + 1] = macro_args;
  macro_stack[macro_stack_ix + 2] = macro_ident;
  macro_stack_ix += 3;

  macro_ident   = ident;
  macro_tok_lst = tokens;
  macro_args    = args;
}

// The macro_is_already_expanding function is buggy and has false positives in
// the repl example. Disable it until we rework macro argument expansion as
// described in https://web.archive.org/web/20250328104901/https://gcc.gnu.org/onlinedocs/cpp/Macro-Arguments.html.
#ifdef ALLOW_RECURSIVE_MACROS
#define macro_is_already_expanding(ident) false
#else
// Search the macro stack to see if the macro is already expanding.
bool macro_is_already_expanding(int ident) {
  int i = macro_stack_ix;
  if (ident == 0 || macro_ident == 0) return false; // Unnamed macro or no macro is expanding
  if (ident == macro_ident)           return true;  // The same macro is already expanding

  // Traverse the stack to see if the macro is already expanding
  while (i > 0) {
    i -= 3;
    if (macro_stack[i + 2] == ident) return true;
  }
  return false;
}
#endif

// Undoes the effect of get_tok by replacing the current token with the previous
// token and saving the current token to be returned by the next call to get_tok.
void undo_token(int prev_tok, int prev_val) {
  begin_macro_expansion(0, cons(cons(tok, val), 0), 0); // Push the current token back
  tok = prev_tok;
  val = prev_val;
}

// Try to expand a macro and returns if the macro was expanded.
// A macro is not expanded if it is already expanding or if it's a function-like
// macro that is not called with parenthesis. In that case, the macro identifier
// is returned as a normal identifier.
// If the wrong number of arguments is passed to a function-like macro, a fatal error is raised.
bool attempt_macro_expansion(int macro) {
  // We must save the tokens because the macro may be redefined while reading the arguments
  int tokens = car(heap[macro + 3]);

  if (macro_is_already_expanding(macro)) { // Self referencing macro
    tok = IDENTIFIER;
    val = macro;
    return false;
  } else if (cdr(heap[macro + 3]) == -1) { // Object-like macro
#ifdef FULL_PREPROCESSOR_SUPPORT
    // Note: Redefining __{FILE,LINE}__ macros, either with the #define or #line directives is not supported.
    if (macro == FILE__ID) {
      tokens = cons(cons(STRING, intern_str(fp_filepath)), 0);
    }
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
    else if (macro == LINE__ID) {
      tokens = cons(cons(INTEGER, -line_number), 0);
    }
#endif
#endif
    begin_macro_expansion(macro, tokens, 0);
    return true;
  } else { // Function-like macro
    get_tok(); // Skip the macro identifier
    if (tok == '(') {
      begin_macro_expansion(macro, tokens, get_macro_args_toks(macro));
      return true;
    } else {
      undo_token(IDENTIFIER, macro);
      return false;
    }
  }
}

#ifdef FULL_PREPROCESSOR_SUPPORT

// https://gcc.gnu.org/onlinedocs/cpp/Stringizing.html
void stringify() {
  int arg;
  expand_macro_arg = false;
  get_tok_macro();
  expand_macro_arg = true;
  if (tok != MACRO_ARG) {
    dump_tok(tok);
    syntax_error("expected macro argument after #");
  }
  arg = get_macro_arg(val);
  tok = car(car(arg));
  // Support the case where the argument is a single identifier/macro/keyword token
  if ((tok == IDENTIFIER || tok == MACRO || (KEYWORDS_START <= tok && tok <= KEYWORDS_END)) && cdr(arg) == 0) {
    val = cdr(car(arg)); // Use the identifier probe
  } else {
    val = NOT_SUPPORTED_ID; // Return string "NOT_SUPPORTED"
  }
  tok = STRING;
}

// Concatenates two non-negative integers into a single integer
// Note that this function only supports small integers, represented as positive integers.
int paste_integers(int left_val, int right_val) {
  int result = left_val;
  int right_digits = right_val;
#ifdef SUPPORT_64_BIT_LITERALS
  if (left_val < 0 || right_val < 0) fatal_error("Only small integers can be pasted");
#endif
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
      begin_macro_expansion(0, get_macro_arg(val), 0); // Play the tokens of the macro argument
      get_tok_macro();
    }
  }
  right_tok = tok;
  right_val = val;
  if (left_tok == IDENTIFIER || left_tok == TYPE || left_tok == MACRO
    || (KEYWORDS_START <= left_tok && left_tok <= KEYWORDS_END)) {
    // Something that starts with an identifier can only be an identifier
    begin_string();
    accum_string_string(left_val);

    if (right_tok == IDENTIFIER || right_tok == TYPE || right_tok == MACRO
     || (KEYWORDS_START <= right_tok && right_tok <= KEYWORDS_END)) {
      accum_string_string(right_val);
    } else if (right_tok == INTEGER
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
            || right_tok == INTEGER_HEX || right_tok == INTEGER_OCT
#endif
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
            || right_tok == INTEGER_L || right_tok == INTEGER_LL || right_tok == INTEGER_U || right_tok == INTEGER_UL || right_tok == INTEGER_ULL
#endif
              ) {
      accum_string_integer(-right_val);
    } else {
      dump_ident(left_val);
      dump_tok(right_tok);
      syntax_error("cannot paste an identifier with a non-identifier or non-negative integer");
    }

    val = end_ident();
    tok = heap[val+2]; // The kind of the identifier
  } else if (left_tok == INTEGER
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
          || left_tok == INTEGER_HEX || left_tok == INTEGER_OCT
#endif
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
          || left_tok == INTEGER_L || left_tok == INTEGER_LL || left_tok == INTEGER_U || left_tok == INTEGER_UL || left_tok == INTEGER_ULL
#endif
            ) {
    if (right_tok == INTEGER
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
     || right_tok == INTEGER_HEX || right_tok == INTEGER_OCT
#endif
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
    || right_tok == INTEGER_L || right_tok == INTEGER_LL || right_tok == INTEGER_U || right_tok == INTEGER_UL || right_tok == INTEGER_ULL
#endif
       ) {
      val = -paste_integers(-left_val, -right_val);
    } else if (right_tok == IDENTIFIER || right_tok == MACRO
            || (KEYWORDS_START <= right_tok && right_tok <= KEYWORDS_END)) {
      begin_string();
      accum_string_integer(-left_val);
      accum_string_string(right_val);

      val = end_ident();
      tok = heap[val+2]; // The kind of the identifier
    } else {
      dump_tok(left_tok);
      dump_tok(right_tok);
      syntax_error("cannot paste an integer with a non-integer");
    }
  } else {
    dump_tok(left_tok);
    dump_tok(right_tok);
    syntax_error("cannot paste a non-identifier or non-integer");
  }
}

#endif // FULL_PREPROCESSOR_SUPPORT

void skip_to_end_of_line() {
  while (ch != '\n' && ch != EOF) {
    get_ch();
  }
}

bool skip_inactive_line() {
  // If the line starts with whitespace followed by a #, it's potentially a preprocessor
  // directive that we need to process even in inactive #if blocks, return true in that case.

  // Skip whitespace
  while (ch <= ' ' && ch != EOF) {
    get_ch();
  }

  if (ch == '#') {
    return true;
  } else {
    // Skip to the end of the line
    skip_to_end_of_line();
    return false;
  }
}

void get_tok() {

#ifdef SH_INCLUDE_C_CODE
  int prev_char_buf_ix = code_char_buf_ix;
  // Save the cursor in a local variable so we can restore it when the token is
  // masked off. Not using the last_tok_code_buf_ix global because get_tok can
  // be called recursively by handle_preprocessor_directive.
  int prev_last_tok_char_buf_ix = code_char_buf_ix;
#endif

#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  int prev_tok_line_number = line_number;
  int prev_tok_column_number = column_number;
#endif

#ifdef SH_INCLUDE_C_CODE
    code_char_buf_ix = prev_char_buf_ix; // Skip over tokens that are masked off
#endif

  while (1) {
    // When if_macro_mask is false, skip all tokens until we reach a preprocessor directive.
    if (!if_macro_mask) {
      while (!skip_inactive_line());
      tok = '\n'; // Return a newline token to end the current directive
    }

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

#ifdef FULL_PREPROCESSOR_SUPPORT
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
        }
      } else if (macro_tok_lst == 0 && paste_last_token) { // We finished expanding the left-hand side of ##
        if (macro_stack_ix == 0) {
          // If we are not in a macro expansion, we can't paste the last token
          // This should not happen if the macro is well-formed, which is
          // checked by read_macro_tokens.
          syntax_error("## cannot appear at the end of a macro expansion");
        }
        return_to_parent_macro();
        paste_last_token = false; // We are done pasting
        paste_tokens(tok, val);
      }
#endif // FULL_PREPROCESSOR_SUPPORT

      if (tok == MACRO) { // Nested macro expansion!
        if (attempt_macro_expansion(val)) {
          continue;
        }
        break;
      } else if (tok == MACRO_ARG && expand_macro_arg) {
        begin_macro_expansion(0, get_macro_arg(val), 0); // Play the tokens of the macro argument
        continue;
      }
#ifdef FULL_PREPROCESSOR_SUPPORT
      else if (tok == '#') { // Stringizing!
        stringify();
        break;
      }
#endif // FULL_PREPROCESSOR_SUPPORT
      break;
    } else if (macro_stack_ix != 0) {
      return_to_parent_macro();
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

      INIT_ACCUM_DIGIT();

      tok = INTEGER;
      if (ch == '0') { // val == 0 <=> ch == '0'
        get_ch();
        if (ch == 'x' || ch == 'X') {
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
          tok = INTEGER_HEX;
#endif
          get_ch();
          if (accum_digit(16)) {
            while (accum_digit(16));
          } else {
            syntax_error("invalid hex integer -- it must have at least one digit");
          }
        } else {
          while (accum_digit(8));
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
          // 0 is a valid octal number, but we don't want to mark it as octal since it's so common
#ifdef SUPPORT_64_BIT_LITERALS
          tok = TERNARY(val_32[0] == 0 && val_32[1] == 0, INTEGER, INTEGER_OCT);
#else
          tok = TERNARY(val == 0, INTEGER, INTEGER_OCT);
#endif
#endif
        }
      } else {
        while (accum_digit(10));
      }

#ifdef SUPPORT_64_BIT_LITERALS
      u64_to_obj(val_32);
#endif

#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
      // If this is enabled with PARSE_NUMERIC_LITERAL_WITH_BASE, using a
      // suffix replaces INTEGER_OCT and INTEGER_HEX with base 10 INTEGER.
      if (ch == 'u' || ch == 'U') {
        // Note: allows suffixes with mixed case, such as lL for simplicity
        tok = INTEGER_U;
        get_ch();
        if (ch == 'l' || ch == 'L') {
          tok = INTEGER_UL;
          get_ch();
          if (ch == 'l' || ch == 'L') {
            tok = INTEGER_ULL;
            get_ch();
          }
        }
      } else if (ch == 'l' || ch == 'L') {
        tok = INTEGER_L;
        get_ch();
        if (ch == 'l' || ch == 'L') {
          tok = INTEGER_LL;
          get_ch();
        }
        if (ch == 'u' || ch == 'U') {
          tok = TERNARY(tok == INTEGER_LL, INTEGER_ULL, INTEGER_UL);
          get_ch();
        }
      }
#endif

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
          skip_to_end_of_line();
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
        }
#ifdef SUPPORT_STRUCT_UNION
        else if (ch == '>') {
          get_ch();
          tok = ARROW;
        }
#endif
        else if (ch == '-') {
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
#ifdef FULL_PREPROCESSOR_SUPPORT
        if (ch == '#') {
          get_ch();
          tok = HASH_HASH;
        }
#endif // FULL_PREPROCESSOR_SUPPORT

        break;

      }
#if defined(SUPPORT_STRUCT_UNION) || defined(SUPPORT_VARIADIC_FUNCTIONS)
      else if (ch == '.') {
        get_ch();
#ifdef SUPPORT_VARIADIC_FUNCTIONS
        if (ch == '.') {
          get_ch();
          if (ch == '.') {
            get_ch();
            tok = ELLIPSIS;
          } else {
            dump_char(ch);
            syntax_error("invalid token");
          }
        }
#endif // SUPPORT_VARIADIC_FUNCTIONS
        break;
      }
#endif
      else if (ch == '~' || ch == '.' || ch == '?' || ch == ',' || ch == ':' || ch == ';' || ch == '(' || ch == ')' || ch == '[' || ch == ']' || ch == '{' || ch == '}') {

        get_ch();

        break;

      } else if (ch == '\\') {
        get_ch();

        if (ch == '\n') { // Continues with next token
          get_ch();
        } else {
          dump_char(ch);
          syntax_error("unexpected character after backslash");
        }
      } else {
        dump_char(ch);
        syntax_error("invalid token");
      }
    }
  }

#ifdef SH_INCLUDE_C_CODE
  last_tok_code_buf_ix = prev_last_tok_char_buf_ix - 1;
#endif

#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  last_tok_line_number = prev_tok_line_number;
  last_tok_column_number = prev_tok_column_number;
#endif
}

// parser
#if defined DEBUG_CPP || defined DEBUG_EXPAND_INCLUDES || defined NICE_ERR_MSG || defined HANDLE_SIGNALS || defined DEBUG_PARSER_SEXP
#include "debug.c"
#endif

void expect_tok(int expected_tok) {
  if (tok != expected_tok) {
#ifdef NICE_ERR_MSG
    putstr("expected tok=");  print_tok_type(expected_tok);
    putstr("\ncurrent tok="); print_tok_type(tok); putchar('\n');
#else
    dump_int("expected tok = ", expected_tok);
    dump_int("current tok = ", tok);
#endif
    parse_error("unexpected token", tok);
  }
  get_tok();
}

ast parse_comma_expression();
ast parse_call_params();
ast parse_cast_expression();
ast parse_compound_statement();
ast parse_conditional_expression();
ast parse_enum();
ast parse_struct_or_union(int struct_or_union_tok);
ast parse_declarator(bool abstract_decl, ast parent_type);
ast parse_declaration_specifiers(bool allow_typedef);
ast parse_initializer_list();
ast parse_initializer();

// The storage class specifier and type qualifier tokens are all together
// following the CONST_KW keyword.
#define MK_TYPE_SPECIFIER(tok) (1 << (tok - CONST_KW))
#define TEST_TYPE_SPECIFIER(specifier, tok) ((specifier) & (1 << (tok - CONST_KW)))

ast get_type_specifier(ast type_or_decl) {
  while (1) {
    switch (get_op(type_or_decl)) {
      case DECL:
        type_or_decl = get_child_(DECL, type_or_decl, 1);
        break;
      case '[':
        type_or_decl = get_child_('[', type_or_decl, 0);
        break;
      case '*':
        type_or_decl = get_child_('*', type_or_decl, 0);
        break;
      default:
        return type_or_decl;
    }
  }
}

ast pointer_type(ast parent_type, bool is_const) {
  return new_ast2('*', TERNARY(is_const, MK_TYPE_SPECIFIER(CONST_KW), 0), parent_type);
}

#ifndef sh

ast function_type(ast parent_type, ast params) {
  return new_ast3('(', parent_type, params, false);
}

#endif

#ifdef SUPPORT_VARIADIC_FUNCTIONS

ast make_variadic_func(ast func_type) {
  set_child(func_type, 2, true); // Set the variadic flag
  return func_type;
}

#endif

#if defined(SH_OPTIMIZE_CONSTANT_PARAMS)
// Used to optimize constant parameters of function
bool is_constant_type(ast type) {
  switch (get_op(type)) {
    case '[': return false; // Array declarators cannot be marked as constant
    case '(': return false; // Function declarators cannot be marked as constant
    case '*': return TEST_TYPE_SPECIFIER(get_child_('*', type, 0), CONST_KW);
    default:  return TEST_TYPE_SPECIFIER(get_child(type, 0), CONST_KW);
  }
}
#else
#define is_constant_type(type) false
#endif

// Type and declaration parser
bool is_type_starter(int tok) {
  switch (tok) {
    case INT_KW: case CHAR_KW: case SHORT_KW: case LONG_KW: // Numeric types
    case VOID_KW: case FLOAT_KW: case DOUBLE_KW:            // Void and floating point types
    case SIGNED_KW: case UNSIGNED_KW:                       // Signedness
    case TYPE:                                              // User defined types
    case CONST_KW:
#ifdef SUPPORT_TYPE_SPECIFIERS
    case VOLATILE_KW:                                       // Type attributes
#endif
    case ENUM_KW:                                           // Enum
#ifdef SUPPORT_STRUCT_UNION
    case STRUCT_KW: case UNION_KW:                          // Struct, union
#endif
    // Storage class specifiers are not always valid type starters in all
    // contexts, but we allow them here
    case TYPEDEF_KW:
#ifdef SUPPORT_TYPE_SPECIFIERS
    case STATIC_KW: case AUTO_KW: case REGISTER_KW: case EXTERN_KW:
    case INLINE_KW:
#endif
      return true;
    default:
      return false;
  }
}

ast parse_enum() {
  ast name;
  ast ident;
  ast result = 0;
  ast rest;
  ast value = 0;
  int next_value = 0;
  int last_literal_type = INTEGER; // Default to decimal integer for enum values

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
        value = parse_assignment_expression();
        if (value == 0) parse_error("Enum value must be a constant expression", tok);

        // Avoid recreating integer literal nodes unnecessarily.
        // Preserve the type of integer literals (dec/hex/oct), we use the last
        // literal type to determine which type to use when creating a new node.
        switch (get_op(value)) {
          case INTEGER:
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
          case INTEGER_HEX: case INTEGER_OCT:
#endif
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
          case INTEGER_U:
          case INTEGER_UL:
          case INTEGER_ULL:
          case INTEGER_L:
          case INTEGER_LL:
#endif
            break;
          default:
            // Evaluate the constant expression to get its integer value
            value = new_ast0(last_literal_type, -eval_constant(value, false)); // negative value to indicate it's a small integer
        }
        next_value = get_val(value) - 1; // Next value is the current value + 1, but val is negative
        last_literal_type = get_op(value);
      } else {
        value = new_ast0(last_literal_type, next_value);
        next_value -= 1;
      }

      if (result == 0) {
        result = cons(new_ast2('=', ident, value), 0);
        rest = result;
      } else {
        set_child(rest, 1, cons(new_ast2('=', ident, value), 0));
        rest = get_child_(LIST, rest, 1);
      }

      if (tok == ',') {
        get_tok();
      } else {
        break;
      }
    }

    expect_tok('}');

  }

  return new_ast3(ENUM_KW, 0, name, result); // child#0 is the storage-class specifiers and type qualifiers
}

#ifdef SUPPORT_STRUCT_UNION

ast parse_struct_or_union(int struct_or_union_tok) {
  ast name;
  ast type_specifier, decl;
  ast result = 0;
  ast rest;
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
      type_specifier = parse_declaration_specifiers(false);

      // If the decl has no name, it's an anonymous struct/union member
      // and there can only be 1 declarator so not looping.
      if (tok == ';') {
        if (get_op(type_specifier) != ENUM_KW && get_op(type_specifier) != STRUCT_KW && get_op(type_specifier) != UNION_KW) {
          parse_error("Anonymous struct/union member must be a struct or union type", tok);
        }
        decl = new_ast3(DECL, 0, type_specifier, 0);

        if (result == 0) {
          rest = result = cons(decl, 0);
        } else {
          set_child(rest, 1, cons(decl, 0));
          rest = get_child_(LIST, rest, 1);
        }
      } else {
        while (1) {
          decl = parse_declarator(false, type_specifier);
          if (result == 0) {
            rest = result = cons(decl, 0);
          } else {
            set_child(rest, 1, cons(decl, 0));
            rest = get_child_(LIST, rest, 1);
          }

          if (get_child_(DECL, decl, 1) == VOID_KW) parse_error("member with void type not allowed in struct/union", tok);
          if (get_child_(DECL, decl, 1) == '[' && get_child_('[', get_child_(DECL, decl, 1), 1) == 0) {
            // Set ends_in_flex_array if the type is an array with no size
            ends_in_flex_array = true;
            break;
          }
          if (tok == ',') get_tok();
          else break;
        }
      }

      expect_tok(';');
    }

    expect_tok('}');
  }

  return new_ast3(struct_or_union_tok, 0, name, result); // child#0 is the storage-class specifiers and type qualifiers
}

#endif // SUPPORT_STRUCT_UNION

ast parse_type_specifier() {
  ast type_specifier = 0;
  switch (tok) {
    case CHAR_KW:
    case INT_KW:
    case VOID_KW:
#ifdef DEBUG_PARSER
    case FLOAT_KW:
    case DOUBLE_KW:
#endif
      type_specifier = new_ast0(tok, 0);
      get_tok();
      return type_specifier;

    case SHORT_KW:
      get_tok();
      if (tok == INT_KW) get_tok(); // Just "short" is equivalent to "short int"
      return new_ast0(SHORT_KW, 0);

    case SIGNED_KW:
      get_tok();
      type_specifier = parse_type_specifier();
      // Just "signed" is equivalent to "signed int"
      if (type_specifier == 0) type_specifier = new_ast0(INT_KW, 0);
      return type_specifier;

#ifndef sh
    case UNSIGNED_KW:
      get_tok();
      type_specifier = parse_type_specifier();
      // Just "unsigned" is equivalent to "unsigned int"
      if (type_specifier == 0) type_specifier = new_ast0(INT_KW, MK_TYPE_SPECIFIER(UNSIGNED_KW));
      // Set the unsigned flag
      else set_val(type_specifier, get_val(type_specifier) | MK_TYPE_SPECIFIER(UNSIGNED_KW));
      return type_specifier;
#endif

    case LONG_KW:
      get_tok();
#ifdef DEBUG_PARSER
      if (tok == DOUBLE_KW) {
        get_tok();
        return new_ast0(DOUBLE_KW, 0);
      } else
#endif
      {
        if (tok == LONG_KW) {
          get_tok();
          if (tok == INT_KW) get_tok(); // Just "long long" is equivalent to "long long int"
          return new_ast0(LONG_KW, 0);
        } else if (tok == INT_KW) {
          get_tok(); // Just "long" is equivalent to "long int", which we treat as "int"
          return new_ast0(INT_KW, 0);
        } else {
          return new_ast0(INT_KW, 0);
        }
      }

    default:
      return 0;
  }
}

// A declaration is split in 2 parts:
//    1. specifiers and qualifiers
//    2. declarators and initializers
// This function parses the first part
// Storage class specifiers affect declarations instead of types, so it's easier to extract it from the type
int glo_specifier_storage_class = 0;
ast parse_declaration_specifiers(bool allow_typedef) {
  ast type_specifier = 0;
  int type_qualifier = 0;
  bool loop = true;
  int specifier_storage_class = 0;

  while (loop) {
    switch (tok) {
#ifdef SUPPORT_TYPE_SPECIFIERS
      case AUTO_KW:
      case REGISTER_KW:
      case STATIC_KW:
      case EXTERN_KW:
#endif
      case TYPEDEF_KW:
        if (specifier_storage_class != 0) fatal_error("Multiple storage classes not supported");
        if (tok == TYPEDEF_KW && !allow_typedef) parse_error("Unexpected typedef", tok);
        specifier_storage_class = tok;
        get_tok();
        break;

#ifdef SUPPORT_TYPE_SPECIFIERS
      case INLINE_KW:
        get_tok(); // Ignore inline
        break;
#endif

      case CONST_KW:
#ifdef SUPPORT_TYPE_SPECIFIERS
      case VOLATILE_KW:
#endif
        type_qualifier |= MK_TYPE_SPECIFIER(tok);
        get_tok();
        break;

      case CHAR_KW:
      case INT_KW:
      case VOID_KW:
      case SHORT_KW:
      case SIGNED_KW:
      case UNSIGNED_KW:
      case LONG_KW:
      case FLOAT_KW:
      case DOUBLE_KW:
        if (type_specifier != 0) parse_error("Unexpected C type specifier", tok);
        type_specifier = parse_type_specifier();
        if (type_specifier == 0) parse_error("Failed to parse type specifier", tok);
        break;

#ifdef SUPPORT_STRUCT_UNION
      case STRUCT_KW:
      case UNION_KW:
        if (type_specifier != 0) parse_error("Multiple types not supported", tok);
        type_specifier = parse_struct_or_union(tok);
        break;
#endif

      case ENUM_KW:
        if (type_specifier != 0) parse_error("Multiple types not supported", tok);
        type_specifier = parse_enum();
        break;

      case TYPE:
        if (type_specifier != 0) parse_error("Multiple types not supported", tok);
        // Lookup type in the types table. It is stored in the tag of the
        // interned string object. The type is cloned so it can be modified.
        type_specifier = clone_ast(heap[val + 3]);
        get_tok();
        break;

      default:
        loop = false; // Break out of loop
        break;
    }
  }

  // Note: Remove to support K&R C syntax
  if (type_specifier == 0) parse_error("Type expected", tok);

  if (type_qualifier != 0) {
    // This can only happen if an array/function type is typedef'ed
    if (get_op(type_specifier) == '[' || get_op(type_specifier) == '(')
      parse_error("Type qualifiers not allowed on typedef'ed array or function type", tok);

    // Set the type qualifier, keeping the storage class specifier from the typedef if it exists
    set_child(type_specifier, 0, get_child(type_specifier, 0) | type_qualifier);
  }
  glo_specifier_storage_class = specifier_storage_class;

  return type_specifier;
}

#ifdef SUPPORT_VARIADIC_FUNCTIONS
bool parse_param_list_is_variadic = false;
#endif
int parse_param_list() {
  ast result = 0;
  ast rest;
  ast decl;

#ifdef SUPPORT_VARIADIC_FUNCTIONS
  parse_param_list_is_variadic = false;
#endif

  expect_tok('(');

  while (tok != ')' && tok != EOF) {
    if (is_type_starter(tok)) {
      decl = parse_declarator(true, parse_declaration_specifiers(false));
      if (get_op(get_child_(DECL, decl, 1)) == VOID_KW) {
        if (tok != ')' || result != 0) parse_error("void must be the only parameter", tok);
        break;
      }
    } else if (tok == IDENTIFIER) {
      // Support K&R param syntax in function definition
      decl = new_ast3(DECL, new_ast0(IDENTIFIER, val), new_ast0(INT_KW, 0), 0);
      get_tok();
    }
#ifdef SUPPORT_VARIADIC_FUNCTIONS
    else if (tok == ELLIPSIS) {
      // ignore ELLIPSIS nodes for now, but it should be the last parameter
      if (result == 0) parse_error("Function must have a named parameter before ellipsis parameter", tok);
      get_tok();
      parse_param_list_is_variadic = true;
      break;
    }
#endif // SUPPORT_VARIADIC_FUNCTIONS
    else {
      parse_error("Parameter declaration expected", tok);
    }

    if (tok == ',') get_tok();

    if (result == 0) {
      rest = result = cons(decl, 0);
    } else {
      set_child(rest, 1, cons(decl, 0));
      rest = get_child_(LIST, rest, 1);
    }
  }

  expect_tok(')');

  return result;
}

ast get_inner_type(ast type) {
  switch (get_op(type)) {
    case DECL:
    case '*':
      return get_child(type, 1);
    case '[':
    case '(':
      return get_child(type, 0);
    default:
      fatal_error("Invalid type");
      return 0;
  }
}

void update_inner_type(ast parent_type, ast inner_type) {
  switch (get_op(parent_type)) {
    case DECL:
    case '*':
      set_child(parent_type, 1, inner_type);
      break;

    case '[':
    case '(':
      set_child(parent_type, 0, inner_type);
      break;
  }
}

// Parse a declarator. In C, declarators are written as they are used, meaning
// that the identifier appears inside the declarator, and is surrounded by the
// operators that are used to access the declared object.
//
// When manipulating declarator and type objects, it's much more convenient to
// have the identifier as the outermost node, and the order of the operators
// reversed, ending with the type specifier (base type).
// For example, `int *a[10]` is parsed as `(decl a (array 10 (pointer int)))`
// even if the parser parses `int`, `*`, identifier and `[10]` in that order.
//
// To achieve this, parse_declarator takes the inner type as an argument, and
// the inner type is extended as the declarator is parsed. The parent_type is
// then used in the declarator base case, the identifier, and which
// creates the DECL node.
//
// There's a small twist to this however, caused by array and function
// declarators appearing postfixed to the declarator. Because tokens are only
// read once, we can't skip ahead to expand the inner type with array/function
// declarator and then recursively call parse_declarator with the extended type.
// Instead, parse_declarator keeps track of the node that wraps the inner type
// and returns it in `parse_declarator_parent_type_parent`. Using the reference
// to the node containing the inner type, it is then possible to insert the
// array/function declarator in the right location, that is around the inner
// type.
//
// Parameters: abstract_decl: true if the declarator may omit the identifier
ast parse_declarator_parent_type_parent;
ast parse_declarator(bool abstract_decl, ast parent_type) {
  bool first_tok = tok; // Indicates if the declarator is a noptr-declarator
  ast result = 0;
  ast decl;
  ast arr_size_expr;
  ast parent_type_parent;

  switch (tok) {
    case IDENTIFIER:
      result = new_ast3(DECL, new_ast0(IDENTIFIER, val), parent_type, 0); // child#2 is the initializer
      parent_type_parent = result;
      get_tok();
      break;

    case '*':
      get_tok();
      // Pointers may be const-qualified
      parent_type_parent = pointer_type(parent_type, tok == CONST_KW);
      if (tok == CONST_KW) get_tok();
      result = parse_declarator(abstract_decl, parent_type_parent);
      break;

    // Parenthesis delimit the specifier-and-qualifier part of the declaration from the declarator
    case '(':
      get_tok();
      result = parse_declarator(abstract_decl, parent_type);
      parent_type_parent = parse_declarator_parent_type_parent;
      expect_tok(')');
      break;

    default:
      // Abstract declarators don't need names, and so in the base declarator,
      // we don't require an identifier. This is useful for function pointers.
      // In that case, we create a DECL node with no identifier.
      if (abstract_decl) {
        result = new_ast3(DECL, 0, parent_type, 0); // child#0 is the identifier, child#2 is the initializer
        parent_type_parent = result;
      } else {
        parse_error("Invalid declarator, expected an identifier but declarator doesn't have one", tok);
      }
  }

  // At this point, the only non-recursive declarator is an identifier
  // so we know that get_op(result) == DECL.
  // Because we want the DECL to stay as the outermost node, we temporarily
  // unwrap the DECL parent_type.
  decl = result;
  result = get_child_(DECL, decl, 1);

  while (first_tok != '*') {
    // noptr-declarator may be followed by [ constant-expression ] to declare an
    // array or by ( parameter-type-list ) to declare a function. We loop since
    // both may be present.
    if (tok == '[') {
      // Check if not a void array
      if (get_op(result) == VOID_KW) parse_error("void array not allowed", tok);
        get_tok();
      if (tok == ']') {
        val = 0;
      } else {
        arr_size_expr = parse_assignment_expression();
        if (arr_size_expr == 0) parse_error("Array size must be an integer constant", tok);
        val = eval_constant(arr_size_expr, false);
      }
      result = new_ast2('[', get_inner_type(parent_type_parent), val);
      update_inner_type(parent_type_parent, result);
      parent_type_parent = result;
      expect_tok(']');
    } else if (tok == '(') {
      result = new_ast3('(', get_inner_type(parent_type_parent), parse_param_list(), false);
#ifdef SUPPORT_VARIADIC_FUNCTIONS
      if (parse_param_list_is_variadic) result = make_variadic_func(result);
#endif
      update_inner_type(parent_type_parent, result);
      parent_type_parent = result;
    } else {
      break;
    }
  }

  parse_declarator_parent_type_parent = parent_type_parent;
  return decl;
}

#ifdef SUPPORT_COMPLEX_INITIALIZER

ast parse_initializer_list() {
  ast result = 0;
  ast rest = 0;

  expect_tok('{');

  while (tok != '}' && tok != EOF) {
#ifdef sh
    if (tok == '{') fatal_error("nested initializer lists not supported");
#endif
    if (result == 0) {
      rest = result = cons(parse_initializer(), 0);
    } else {
      set_child(rest, 1, cons(parse_initializer(), 0));
      rest = get_child_(LIST, rest, 1);
    }
    if (tok == ',') get_tok();
    else break;
  }

  expect_tok('}');

  return new_ast1(INITIALIZER_LIST, result);
}

#endif // SUPPORT_COMPLEX_INITIALIZER

ast parse_initializer() {
#ifdef SUPPORT_COMPLEX_INITIALIZER
  if (tok == '{') {
    return parse_initializer_list();
  } else {
    return parse_assignment_expression();
  }
#else
  return parse_assignment_expression();
#endif
}

ast parse_declarator_and_initializer(bool is_for_typedef, ast type_specifier) {
  ast declarator = parse_declarator(false, type_specifier);

  if (is_for_typedef == 0) {
    if (tok == '=') {
      get_tok();
      // parse_declarator returns a DECL node where the initializer is child#2
      set_child(declarator, 2, parse_initializer());
    }
  }

  return declarator;
}

ast parse_declarators(bool is_for_typedef, ast type_specifier, ast first_declarator) {
  ast declarators = cons(first_declarator, 0); // Wrap the declarators in a list
  ast rest = declarators;

  // Otherwise, this is a variable or declaration
  while (tok != ';') {
    if (tok == ',') {
      get_tok();
      set_child(rest, 1, cons(parse_declarator_and_initializer(is_for_typedef, type_specifier), 0));
      rest = get_child__(LIST, LIST, rest, 1);
    } else {
      parse_error("';' or ',' expected", tok);
    }
  }

  return declarators;
}

void add_typedef(ast declarator) {
  int decl_ident = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, declarator, 0));
  ast decl_type = get_child_(DECL, declarator, 1); // child#1 is the type

#if defined(SUPPORT_STRUCT_UNION) && defined(sh)
  // If the struct/union/enum doesn't have a name, we give it the name of the typedef.
  // This is not correct, but it's a limitation of the current shell backend where we
  // need the name of a struct/union/enum to compile sizeof and typedef'ed structures
  // don't always have a name.
  if (get_op(decl_type) == STRUCT_KW || get_op(decl_type) == UNION_KW || get_op(decl_type) == ENUM_KW) {
    if (get_child(decl_type, 1) != 0 && get_val_(IDENTIFIER, get_child(decl_type, 1)) != decl_ident) {
      syntax_error("typedef name must match struct/union/enum name");
    }
    set_child(decl_type, 1, new_ast0(IDENTIFIER, decl_ident));
  }
#endif

  heap[decl_ident + 2] = TYPE;
  heap[decl_ident + 3] = decl_type;
}

ast parse_fun_def(ast declarator) {
  ast fun_type = get_child__(DECL, '(', declarator, 1);
  ast params = get_child_('(', fun_type, 1);

  // Check that the parameters are all named since declarator may be abstract
  while (params != 0) {
    if (get_child_(DECL, get_child__(LIST, DECL, params, 0), 0) == 0) {
      parse_error("Parameter name expected", tok);
    }
    params = get_child_(LIST, params, 1);
  }
  if (get_child_(DECL, declarator, 2) != 0) parse_error("Initializer not allowed in function definition", tok);
  return new_ast2(FUN_DECL, declarator, parse_compound_statement());
}

ast parse_declaration(bool local) {
  ast result;
  ast declarator;
  ast declarators;
  // First we parse the specifiers:
  ast type_specifier = parse_declaration_specifiers(true);

  // From cppreference:
  // > The enum, struct, and union declarations may omit declarators, in which
  // > case they only introduce the enumeration constants and/or tags.
  if (tok == ';') {
    if (get_op(type_specifier) != ENUM_KW
#ifdef SUPPORT_STRUCT_UNION
    && get_op(type_specifier) != STRUCT_KW && get_op(type_specifier) != UNION_KW
#endif
      ) {
      parse_error("enum/struct/union declaration expected", tok);
    }
    // If the specifier is a typedef, we add the typedef'ed type to the type table
    // Note: Should this return a DECL node instead of a ENUM, STRUCT, or UNION node?
    // It doesn't have a name so maybe it makes more sense to have a separate node type?
    if (glo_specifier_storage_class == TYPEDEF_KW) add_typedef(new_ast3(DECL, 0, type_specifier, 0));
    result = type_specifier;
  } else if (glo_specifier_storage_class == TYPEDEF_KW) {
    // The type_specifier contained a typedef, it can't be a function or a
    // variable declaration, and the declarators cannot be initialized.
    // The typedef'ed types will be added to the type table.
    declarator = parse_declarator_and_initializer(true, type_specifier); // First declarator
    declarators = parse_declarators(true, type_specifier, declarator);
    type_specifier = declarators; // Save declarators in type_specifier
    while (declarators != 0) {
      add_typedef(get_child__(LIST, DECL, declarators, 0));
      declarators = get_child_opt_(LIST, LIST, declarators, 1);
    }
    result = new_ast1(TYPEDEF_KW, type_specifier);
  } else {
    // Then we parse the declarators and initializers
    declarator = parse_declarator_and_initializer(false, type_specifier);

    // The declarator may be a function definition, in which case we parse the function body
    if (get_op(get_child_(DECL, declarator, 1)) == '(' && tok == '{') {
      if (local) parse_error("Function definition not allowed in local scope", tok);
      return parse_fun_def(declarator);
    }

    declarators = parse_declarators(false, type_specifier, declarator);
    result = new_ast2(DECLS, declarators, glo_specifier_storage_class); // child#1 is the storage class specifier
  }

  expect_tok(';');
  return result;
}

ast parse_parenthesized_expression(bool first_par_already_consumed) {

  ast result;

  if (!first_par_already_consumed) {
    expect_tok('(');
  }

  result = parse_comma_expression();

  expect_tok(')');

  return result;
}

ast parse_primary_expression() {

  ast result = 0;
  ast rest;

  if (tok == IDENTIFIER || tok == CHARACTER || tok == INTEGER
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
     || tok == INTEGER_HEX || tok == INTEGER_OCT
#endif
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
     || tok == INTEGER_L ||  tok == INTEGER_LL ||  tok == INTEGER_U ||  tok == INTEGER_UL ||  tok == INTEGER_ULL
#endif
     ) {

    result = new_ast0(tok, val);
    get_tok();

  } else if (tok == STRING) {
    result = new_ast0(STRING, val);
    get_tok();

    if (tok == STRING) { // Contiguous strings
      result = cons(get_val_(STRING, result), 0); // Result is now a list of string values
      rest = result;
      while (tok == STRING) {
        set_cdr(rest, cons(val, 0));
        rest = cdr(rest);
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

    result = parse_parenthesized_expression(false);

  } else {
    parse_error("identifier, literal, or '(' expected", tok);
  }

  return result;
}

ast parse_postfix_expression(ast result) {
  ast child;

  if (result == 0) {
    result = parse_primary_expression();
  }

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
        child = parse_call_params();
      }
      result = new_ast2('(', result, child);
      expect_tok(')');
    }
#ifdef SUPPORT_STRUCT_UNION
    else if (tok == '.') {

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

    }
#endif // SUPPORT_STRUCT_UNION
    else if (tok == PLUS_PLUS) {

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

// When parsing parenthesized types (in casts and sizeof), a '(' token is
// consumed and then the next token is checked to see if it begins a type or if
// we're really just parsing a parenthesized expression. When it's a
// parenthesized expression, we need to backtrack and try parsing a unary
// expression. That used to be done by putting the '(' token back on the token
// stream and calling parse_unary_expression.
//
// Having to push back tokens on the stream consumes memory and means our parser
// isn't truely one-pass. To avoid having to push back the '(' token, we can
// instead call the parse_parenthesized_expression function directly, followed
// by the parse_unary_expression function. These 2 function calls correspond to
// the functions that would be called if we had pushed back the '(' token and
// called parse_unary_expression.
#define parse_unary_expression_with_parens_prefix() \
  parse_postfix_expression(parse_parenthesized_expression(true));

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

#ifdef SUPPORT_SIZEOF
  } else if (skip_newlines && tok == SIZEOF_KW) { // only parse sizeof if we're not in a #if expression

    get_tok();
    if (tok == '(') {
      get_tok();
      // May be a type or an expression
      if (is_type_starter(tok)) {
        result = parse_declarator(true, parse_declaration_specifiers(false));
        expect_tok(')');
      } else {
        result = parse_unary_expression_with_parens_prefix();
      }
    } else {
      result = parse_unary_expression();
    }
    result = new_ast1(SIZEOF_KW, result);

#endif // SUPPORT_SIZEOF
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
    result = parse_postfix_expression(0);
  }

  return result;
}

ast parse_cast_expression() {
  ast result;
  ast type;

  if (tok == '(') {
    get_tok();

    if (is_type_starter(tok)) {
      type = parse_declarator(true, parse_declaration_specifiers(false));

      expect_tok(')');
      result = new_ast2(CAST, type, parse_cast_expression());
      return result;
    } else {
      return parse_unary_expression_with_parens_prefix();
    }
  } else {
    return parse_unary_expression();
  }
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

ast parse_call_params() {
  ast result = parse_assignment_expression();
  result = cons(result, 0);

  if (tok == ',') {
    get_tok();
    set_child(result, 1, parse_call_params());
  }

  return result;
}

ast parse_comma_expression_opt() {

  if (tok == ':' || tok == ';' || tok == ')') {
    return 0;
  } else {
    return parse_comma_expression();
  }
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
    result = parse_parenthesized_expression(false);
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
    result = parse_parenthesized_expression(false);
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
    result = parse_parenthesized_expression(false);
    child1 = parse_statement();

    result = new_ast2(WHILE_KW, result, child1);

#ifdef SUPPORT_DO_WHILE
  } else if (tok == DO_KW) {

    get_tok();
    result = parse_statement();
    expect_tok(WHILE_KW);
    child1 = parse_parenthesized_expression(false);
    expect_tok(';');

    result = new_ast2(DO_KW, result, child1);
#endif // SUPPORT_DO_WHILE

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

#ifdef SUPPORT_GOTO
  } else if (tok == GOTO_KW) {

    get_tok();
    expect_tok(IDENTIFIER);
    result = new_ast1(GOTO_KW, new_ast0(IDENTIFIER, val));
    expect_tok(';');

#endif // SUPPORT_GOTO

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
  ast rest;

  expect_tok('{');

  while (tok != '}' && tok != EOF) {
    if (is_type_starter(tok)) {
      child1 = parse_declaration(true);
    } else {
      child1 = parse_statement();
    }

    // Skip empty statements (semicolon)
    if (child1 == 0) continue;

    if (result == 0) {
      result = new_ast2('{', child1, 0);
      rest = result;
    } else {
      child1 = new_ast2('{', child1, 0);
      set_child(rest, 1, child1);
      rest = child1;
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

#ifdef FULL_CLI_OPTIONS

void handle_macro_D(char *opt) {
  char *start = opt;
  char *macro_buf;
  char *buf2;
  int acc;
  while (*opt != 0 && *opt != '=') opt += 1; // Find = sign if any

  macro_buf = malloc(opt - start + 1);
  memcpy(macro_buf, start, opt - start);
  macro_buf[opt - start] = '\0';

  if (*opt == '=') {
    opt += 1;
    if (*opt == '"') { // Start of string literal
      opt += 1;
      start = opt;
      while (*opt != 0 && *opt != '"') opt += 1;
      if (*opt == 0) fatal_error("Unterminated string literal");
      buf2 = malloc(opt - start + 1);
      memcpy(buf2, start, opt - start);
      buf2[opt - start] = '\0';
      init_builtin_string_macro(macro_buf, buf2);
      free(buf2);
    } else if ('0' <= *opt && *opt <= '9') { // Start of integer token
      acc = 0;
      while ('0' <= *opt && *opt <= '9') {
        acc *= 10;
        acc += *opt - '0';
        opt += 1;
      }
      if (*opt != 0) fatal_error("Invalid macro definition value");
      init_builtin_int_macro(macro_buf, acc);
    } else if (*opt == '\0') { // No value given, empty macro
      init_builtin_empty_macro(macro_buf);
    } else {
      fatal_error("Invalid macro definition value");
    }
  } else {
    // Default to 1 when no value is given
    init_builtin_int_macro(macro_buf, 1);
  }

  free(macro_buf);
}

#endif // FULL_CLI_OPTIONS

int main(int argc, char **argv) {
  int i;
  ast decl;

#ifdef HANDLE_SIGNALS
  signal(SIGINT, signal_callback_handler);
#endif

  init_ident_table();

  init_pnut_macros();

  for (i = 1; i < argc; i += 1) {
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {
#ifndef sh
        case 'o':
          // Output file name
          if (argv[i][2] == 0) { // rest of option is in argv[i + 1]
            if (argv[i + 1] == 0) fatal_error("missing output file name for -o option");
            i += 1;
            output_fd = open(argv[i], O_WRONLY | O_CREAT | O_TRUNC, 0755);
          } else {
            output_fd = open(argv[i] + 2, O_WRONLY | O_CREAT | O_TRUNC, 0755);
          }
          break;
#endif

#ifdef FULL_CLI_OPTIONS
        case 'D':
          if (argv[i][2] == 0) { // rest of option is in argv[i + 1]
            if (argv[i + 1] == 0) fatal_error("missing macro name for -D option");
            i += 1;
            handle_macro_D(argv[i]);
          } else {
            handle_macro_D(argv[i] + 2); // skip '-D'
          }
          break;

        case 'U':
          if (argv[i][2] == 0) { // rest of option is in argv[i + 1]
            if (argv[i + 1] == 0) fatal_error("missing macro name for -U option");
            i += 1;
            init_ident(IDENTIFIER, argv[i]);
          } else {
            init_ident(IDENTIFIER, argv[i] + 2); // skip '-U'
          }
          break;

        case 'I':
          if (include_search_path != 0) fatal_error("only one include path allowed");

          if (argv[i][2] == 0) { // rest of option is in argv[i + 1]
            if (argv[i + 1] == 0) fatal_error("missing path for -I option");
            i += 1;
            include_search_path = argv[i];
          } else {
            include_search_path = argv[i] + 2; // skip '-I'
          }
          break;
#else
          case 'D':
            // pnut-sh only needs -D<macro> and no other options
            init_builtin_int_macro(argv[i] + 2, 1); // +2 to skip -D
            break;
#endif // FULL_CLI_OPTIONS
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
    decl = parse_declaration(false);
#ifdef DEBUG_PARSER_SEXP
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
    printf("# %s:%d:%d\n", fp_filepath, line_number, column_number);
#endif
    ast_to_sexp(decl);
    putchar('\n');
#endif
  }
#else
  codegen_begin();
  get_tok();
  while (tok != EOF) {
    decl = parse_declaration(false);
#ifdef SH_INCLUDE_C_CODE
    output_declaration_c_code(get_op(decl) == '=' | get_op(decl) == DECLS);
#endif
    codegen_glo_decl(decl);
  }
  codegen_end();
#endif

  return 0;
}
