// POSIX shell codegen

#include "sh-runtime.c"

#ifdef SH_SUPPORT_SHELL_INCLUDE

void handle_shell_include() {
  int c;
  if (tok == STRING) {
    // Include pack_string and unpack_string functions
    // since they will likely be used in the included file
    runtime_use_put_pstr = true;
    runtime_use_unpack_string = true;
    // Include the file as-is without any preprocessing
    include_file(STRING_BUF(val), fp_dirname);
    while ((c = fgetc(fp)) != EOF) {
      putchar(c);
    }
    putchar('\n');
    restore_include_context();
    get_tok_macro(); // Skip the string
  } else {
    dump_tok(tok);
    syntax_error("expected string to #include_shell directive");
  }
}

#endif // SH_SUPPORT_SHELL_INCLUDE

// codegen

#define text int
#define TEXT_POOL_SIZE 1000000
intptr_t text_pool[TEXT_POOL_SIZE];
int text_alloc = 1; // Start at 1 because 0 is the empty text

// Text pool nodes
enum TEXT_NODES {
  TEXT_TREE,        // Concatenation of texts
  TEXT_INTEGER,     // Integer to be printed in decimal
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
  TEXT_INTEGER_HEX, // Integer to be printed in hexadecimal
  TEXT_INTEGER_OCT, // Integer to be printed in octal
#endif
  TEXT_STRING,      // Pointer to immutable string
  TEXT_ESCAPED      // Escaped string, used for printf
};

// Place prototype of mutually recursive functions here

typedef enum STMT_CTX {
  // Default context
  STMT_CTX_DEFAULT      = 0,
  // Indicates that the parent statement was a else statement so that if
  // statement uses elif instead of if.
  STMT_CTX_ELSE_IF      = 1,
  // Indicates that we are in a switch statement where breaks mean the end of
  // the conditional block.
  STMT_CTX_SWITCH       = 2,
} STMT_CTX;

text comp_lvalue_address(ast node);
text comp_lvalue(ast node);
text comp_fun_call_code(ast node, ast assign_to);
void comp_fun_call(ast node, ast assign_to);
bool comp_body(ast node, STMT_CTX stmt_ctx);
bool comp_statement(ast node, STMT_CTX stmt_ctx);
void mark_mutable_variables_body(ast node);
void handle_enum_struct_union_type_decl(ast node);
ast handle_side_effects_go(ast node, int executes_conditionally);

// Because concatenating strings is very expensive and a common operation, we
// use a tree structure to represent the concatenated strings. That way, the
// concatenation can be done in O(1).
// At the end of the codegen process, the tree will be flattened into a single
// string.

// A few macros to help us change the representation of text objects
#define TEXT_FROM_INT(i)  i
#define TEXT_FROM_CHAR(i) i
#define TEXT_FROM_PTR(p)  ((intptr_t) (p))
#define TEXT_TO_INT(p)    ((int)      (p))
#define TEXT_TO_CHAR(p)   ((char)     (p))

#define wrap_char(c) (-c)

text wrap_int(const int i) {
  if (text_alloc + 2 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_INTEGER);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(i);
  return (text_alloc += 2) - 2;
}

#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE

text wrap_int_hex(const int i) {
  if (text_alloc + 2 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_INTEGER_HEX);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(i);
  return (text_alloc += 2) - 2;
}

text wrap_int_oct(const int i) {
  if (text_alloc + 2 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_INTEGER_OCT);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(i);
  return (text_alloc += 2) - 2;
}

text wrap_integer(const int multiply, const int obj) {
  switch (get_op(obj)) {
    case INTEGER:
      return wrap_int(multiply * -get_val_(INTEGER, obj));
    case INTEGER_HEX:
      return wrap_int_hex(multiply * -get_val_(INTEGER_HEX, obj));
    case INTEGER_OCT:
      return wrap_int_oct(multiply * -get_val_(INTEGER_OCT, obj));
    default:
      fatal_error("wrap_integer: unknown integer type");
      return 0;
  }
}
#else
#define wrap_integer(multiply, obj) wrap_int(multiply * -get_val_(INTEGER, obj))
#endif

text escape_text(const text t, const bool for_printf) {
  if (text_alloc + 3 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");

  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_ESCAPED);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(t);
  text_pool[text_alloc + 2] = TEXT_FROM_INT(for_printf);
  return (text_alloc += 3) - 3;
}

text string_concat(const text t1, const text t2) {
  if (text_alloc + 4 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_TREE);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(2);
  text_pool[text_alloc + 2] = TEXT_FROM_INT(t1);
  text_pool[text_alloc + 3] = TEXT_FROM_INT(t2);
  return (text_alloc += 4) - 4;
}

text string_concat3(const text t1, const text t2, const text t3) {
  if (text_alloc + 5 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_TREE);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(3);
  text_pool[text_alloc + 2] = TEXT_FROM_INT(t1);
  text_pool[text_alloc + 3] = TEXT_FROM_INT(t2);
  text_pool[text_alloc + 4] = TEXT_FROM_INT(t3);
  return (text_alloc += 5) - 5;
}

text string_concat4(const text t1, const text t2, const text t3, const text t4) {
  if (text_alloc + 6 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_TREE);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(4);
  text_pool[text_alloc + 2] = TEXT_FROM_INT(t1);
  text_pool[text_alloc + 3] = TEXT_FROM_INT(t2);
  text_pool[text_alloc + 4] = TEXT_FROM_INT(t3);
  text_pool[text_alloc + 5] = TEXT_FROM_INT(t4);
  return (text_alloc += 6) - 6;
}

text string_concat5(const text t1, const text t2, const text t3, const text t4, const text t5) {
  if (text_alloc + 7 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_TREE);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(5);
  text_pool[text_alloc + 2] = TEXT_FROM_INT(t1);
  text_pool[text_alloc + 3] = TEXT_FROM_INT(t2);
  text_pool[text_alloc + 4] = TEXT_FROM_INT(t3);
  text_pool[text_alloc + 5] = TEXT_FROM_INT(t4);
  text_pool[text_alloc + 6] = TEXT_FROM_INT(t5);
  return (text_alloc += 7) - 7;
}

// Dead code but keeping it around in case we need to wrap mutable strings
// text wrap_str(char * const s) {
//   int i = 0;
//   int result = text_alloc;
//
//   text_pool[result] = TEXT_FROM_INT(TEXT_TREE);
//   text_alloc += 2;
//   while (s[i] != 0) {
//     text_pool[text_alloc] = wrap_char(s[i]);
//     text_alloc += 1;
//     i += 1;
//   }
//
//   text_pool[result + 1] = TEXT_FROM_INT(i);
//
//   return result;
// }

// Like wrap_str, but assumes that the string is immutable and doesn't need to be copied
text wrap_str_imm(char * const s, char * const end) {
  if (text_alloc + 3 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_STRING);
  text_pool[text_alloc + 1] = TEXT_FROM_PTR(s);
  text_pool[text_alloc + 2] = TEXT_FROM_PTR(end); // end of string address. 0 for null-terminated strings
  return (text_alloc += 3) - 3;
}

text wrap_str_lit(char * const s) {
  return wrap_str_imm(s, 0);
}

text wrap_str_pool(const int ident_probe) {
  return wrap_str_imm(STRING_BUF(ident_probe), 0);
}

text concatenate_strings_with(const text t1, const text t2, const text sep) {
  if (t1 == 0) return t2;
  if (t2 == 0) return t1;
  return string_concat3(t1, sep, t2);
}

void print_escaped_char(char c, int for_printf) {
  // C escape sequences
  if      (c == '\0') { putchar('\\');  putchar('0'); }
  else if (c == '\a') { putchar('\\');  putchar('a'); }
  else if (c == '\b') { putchar('\\');  putchar('b'); }
  else if (c == '\f') { putchar('\\');  putchar('f'); }
  else if (c == '\n') { putchar('\\');  putchar('n'); }
  else if (c == '\r') { putchar('\\');  putchar('r'); }
  else if (c == '\t') { putchar('\\');  putchar('t'); }
  else if (c == '\v') { putchar('\\');  putchar('v'); }
  // backslashes are escaped twice, first by the shell and then by def_str
  else if (c == '\\') { putchar('\\');  putchar('\\'); putchar('\\'); putchar('\\'); }
  // Shell special characters: $, `, ", ', ?, and newline
  // Note that ' and ? are not escaped properly by dash, but that's ok because
  // we use double quotes and ' and ? can be left as is.
  else if (c == '$')  { putchar('\\'); putchar('$');  }
  else if (c == '`')  { putchar('\\'); putchar('`');  }
  else if (c == '"')  { putchar('\\'); putchar('"');  }
  // else if (c == '\'') { putchar('\\'); putchar('\''); }
  // else if (c == '?')  { putchar('\\'); putchar('?');  }
  // when we're escaping a string for shell's printf, % must be escaped
  else if (c == '%'  && for_printf) { putchar('%'); putchar('%'); }
  else                putchar(c);
}

void print_escaped_string(char *string_start, char *string_end, int for_printf) {
  if (string_end) {
    while (string_start < string_end) {
      print_escaped_char(*string_start, for_printf);
      string_start += 1;
    }
  } else {
    while (*string_start != 0) {
      print_escaped_char(*string_start, for_printf);
      string_start += 1;
    }
  }
}

void print_escaped_text(text t, bool for_printf) {
  int i;

  if (t == 0) return;

  if (t < 0) { // it's a character
    print_escaped_char(-t, for_printf);
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_TREE)) {
    i = 0;
    while (TEXT_FROM_INT(i) < text_pool[t + 1]) {
      if (text_pool[t + i + 2] < 0) {
        print_escaped_char(-TEXT_TO_CHAR(text_pool[t + i + 2]), for_printf);
      } else {
        print_escaped_text(TEXT_TO_INT(text_pool[t + i + 2]), for_printf);
      }
      i += 1;
    }
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_INTEGER)) {
    putint(TEXT_TO_INT(text_pool[t + 1]));
  }
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
  else if (text_pool[t] == TEXT_FROM_INT(TEXT_INTEGER_HEX)) {
    putchar('0'); putchar('x');
    puthex_unsigned(TEXT_TO_INT(text_pool[t + 1]));
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_INTEGER_OCT)) {
    putchar('0'); // Note: This is not supported by zsh by default
    putoct_unsigned(TEXT_TO_INT(text_pool[t + 1]));
  }
#endif
  else if (text_pool[t] == TEXT_FROM_INT(TEXT_STRING)) {
    print_escaped_string((char*) text_pool[t + 1],  (char*) text_pool[t + 2], for_printf);
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_ESCAPED)) {
    fatal_error("Cannot escape a string that is already escaped");
  } else {
    fatal_error("print_escaped_text: unexpected string tree node");
  }
}

void print_text(text t) {
  int i;
  char *s;

  if (t == 0) return;

  if (t < 0) { // it's a character
    putchar(-t);
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_TREE)) {
    i = 0;
    while (TEXT_FROM_INT(i) < text_pool[t + 1]) {
      if (text_pool[t + i + 2] < 0) {
        putchar(-TEXT_TO_CHAR(text_pool[t + i + 2]));
      } else {
        print_text(TEXT_TO_INT(text_pool[t + i + 2]));
      }
      i += 1;
    }
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_INTEGER)) {
    putint(TEXT_TO_INT(text_pool[t + 1]));
  }
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
  else if (text_pool[t] == TEXT_FROM_INT(TEXT_INTEGER_HEX)) {
    putchar('0'); putchar('x');
    puthex_unsigned(TEXT_TO_INT(text_pool[t + 1]));
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_INTEGER_OCT)) {
    putchar('0'); // Note: This is not supported by zsh by default
    putoct_unsigned(TEXT_TO_INT(text_pool[t + 1]));
  }
#endif
  else if (text_pool[t] == TEXT_FROM_INT(TEXT_STRING)) {
    if (TEXT_TO_INT(text_pool[t + 2]) == 0) { // null-terminated string
      putstr((char*) text_pool[t + 1]);
    } else { // string ends at the address in text_pool[t + 2]
      s = (char*) text_pool[t + 1]; // start
      while (s < (char*) text_pool[t + 2] || *s != 0) {
        putchar(*s);
        s += 1;
      }
    }
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_ESCAPED)) {
    print_escaped_text(TEXT_TO_INT(text_pool[t + 1]), TEXT_TO_INT(text_pool[t + 2]));
  } else {
    fatal_error("print_text: unexpected string tree node");
  }
}

// Codegen context

#define GLO_DECL_SIZE 100000
text glo_decls[GLO_DECL_SIZE];  // Generated code
int glo_decl_ix = 0;            // Index of last generated line of code
int nest_level = 0;             // Current level of indentation
int in_tail_position = false;   // Is the current statement in tail position?
int gensym_ix = 0;              // Counter for fresh_ident
int fun_gensym_ix = 0;          // Maximum value of gensym_ix for the current function
int max_gensym_ix = 0;          // Maximum value of gensym_ix for all functions
int string_counter = 0;         // Counter for string literals
ast rest_loc_var_fixups = 0;    // rest_loc_vars call to fixup after compiling a function
bool main_defined = false;      // If the main function is defined
bool top_level_stmt = true;     // If the current statement is at the top level

#define CHARACTERS_BITFIELD_SIZE 16
int characters_useds[16];        // Characters used in string literals. Bitfield, each int stores 16 bits, so 16 ints in total
bool any_character_used = false; // If any character is used

// Internal identifier node types used by the compiler
enum IDENTIFIER_TYPE {
  IDENTIFIER_INTERNAL = 600,  // Temporary variable
  IDENTIFIER_STRING,          // String literal variable
  IDENTIFIER_DOLLAR           // $ variable
};

// Pre-allocated internal identifier nodes to reduce memory usage.
#define IDENTIFIER_INTERNAL_PREALLOC_SIZE 10
int preallocated_fresh_idents[IDENTIFIER_INTERNAL_PREALLOC_SIZE]; // 1 to 10

#define IDENTIFIER_DOLLAR_PREALLOC_SIZE 10 // 1 to 10
int preallocated_dollar_idents[IDENTIFIER_DOLLAR_PREALLOC_SIZE]; // 1 to 10

void init_comp_context() {
  int i = 0;
  // Initialize characters_useds table
  while (i < 16) {
    characters_useds[i] = 0;
    i += 1;
  }

#ifdef SH_INCLUDE_ALL_ALPHANUM_CHARACTERS
  // Mark all alphanum characters as used: A-Z, a-z, 0-9
  any_character_used = true;
  characters_useds[3] = 0x03FF; // 0-9 (bit 48 to 57)
  characters_useds[4] = 0xFFFE; // A-O (bit 65 to 79)
  characters_useds[5] = 0x07FF; // Q-Z (bit 80 to 90)
  characters_useds[6] = 0xFFFE; // A-O (bit 65 to 79)
  characters_useds[7] = 0x07FF; // Q-Z (bit 80 to 90)
#endif

  // Initialize preallocated_fresh_idents
  i = 0;
  while (i < IDENTIFIER_INTERNAL_PREALLOC_SIZE) {
    preallocated_fresh_idents[i] = new_ast0(IDENTIFIER_INTERNAL, i);
    i += 1;
  }

  // Initialize preallocated_dollar_idents
  i = 0;
  while (i < IDENTIFIER_DOLLAR_PREALLOC_SIZE) {
    preallocated_dollar_idents[i] = new_ast0(IDENTIFIER_DOLLAR, i);
    i += 1;
  }
}

void append_glo_decl(text decl) {
  glo_decls[glo_decl_ix] = nest_level;
  glo_decls[glo_decl_ix + 1] = 1; // If it's active or not. Used by undo_glo_decls and replay_glo_decls
  glo_decls[glo_decl_ix + 2] = decl;
  glo_decl_ix += 3;
}

int append_glo_decl_fixup() {
  glo_decls[glo_decl_ix] = -nest_level - 1; // Negative value to indicate that it's a fixup
  glo_decls[glo_decl_ix + 1] = 1; // If it's active or not. Used by undo_glo_decls and replay_glo_decls
  glo_decls[glo_decl_ix + 2] = 0;
  glo_decl_ix += 3;
  return glo_decl_ix - 3;
}

void fixup_glo_decl(int fixup_ix, text decl) {
  if (glo_decls[fixup_ix] >= 0) fatal_error("fixup_glo_decl: invalid fixup");

  glo_decls[fixup_ix] = -glo_decls[fixup_ix] - 1; // Make nest level positive
  glo_decls[fixup_ix + 2] = decl;
}

// Remove the n last declarations by decrementing the active field.
// A non-positive active value means that the declaration is active,
// A 0 value means that the declaration was unset once.
// A negative value means that the declaration was unset multiple times.
// Because undone declarations are generally replayed, declarations with negative
// values are ignored when replayed since they have already been replayed before.
// This is useful to compile some code at a different time than it is used.
void undo_glo_decls(int start) {
  while (start < glo_decl_ix) {
    glo_decls[start + 1] -= 1; // To support nested undone declarations
    start += 3;
  }
}

// Check if there are any active and non-empty declarations since the start index.
// This is used to determine if a ':' statement must be added to the current block.
bool any_active_glo_decls(int start) {
  while (start < glo_decl_ix) {
    if (glo_decls[start + 1] && glo_decls[start + 2] != 0) return true;
    start += 3;
  }
  return false;
}

// Replay the declarations betwee start and end. Replayed declarations must first
// be undone with undo_glo_decls.
// The reindent parameter controls if the declarations should be replayed at the
// current nest level or at the nest level when they were added.
void replay_glo_decls(int start, int end, int reindent) {
  while (start < end) {
    if (glo_decls[start + 1] == 0) { // Skip inactive declarations that are at the current level
      append_glo_decl(glo_decls[start + 2]);
      if (!reindent) glo_decls[glo_decl_ix - 3] = glo_decls[start]; // Replace nest_level
    }
    start += 3;
  }
}

text replay_glo_decls_inline(int start, int end) {
  text res = 0;
  while (start < end) {
    if (glo_decls[start + 1] == 0) { // Skip inactive declarations
      res = concatenate_strings_with(res, glo_decls[start + 2], wrap_str_lit("; "));
    }
    start += 3;
  }
  if (res != 0) { res = string_concat(res, wrap_str_lit("; ")); }

  return res;
}

void print_glo_decls() {
  int i = 0;
  int level;
  while (i < glo_decl_ix) {
    if (glo_decls[i + 1] == 1) { // Skip inactive declarations
      if (glo_decls[i + 2] != 0) {
        level = glo_decls[i];
        while (level > 0) {
          putchar(' '); putchar(' ');
          level -= 1;
        }
        print_text(glo_decls[i + 2]);
        putchar('\n');
      }
    }
    i += 3;
  }
}

// Environment tracking
#include "env.c"

#define INTERNAL_VAR_FORMAT(x) string_concat(wrap_str_lit("__t"), wrap_int(x))
#define STRING_VAR_FORMAT(x)   string_concat(wrap_str_lit("__str_"), wrap_int(x))

// Similar to env_var, but doesn't use the local environment and assumes that the
// identifier is internal or global. This is faster than env_var when we know that
// the variable is not local.
text format_special_var(ast ident, bool prefixed_with_dollar) {
  int i;
  switch (get_op(ident)) {
    case IDENTIFIER_INTERNAL:
      return INTERNAL_VAR_FORMAT(get_val_(IDENTIFIER_INTERNAL, ident));
    case IDENTIFIER_STRING:
      return STRING_VAR_FORMAT(get_val_(IDENTIFIER_STRING, ident));
    case IDENTIFIER_DOLLAR:
      i = get_val_(IDENTIFIER_DOLLAR, ident);
      if (prefixed_with_dollar) {
          if (i <= 9) {
            return wrap_int(i);
        } else {
            return string_concat3(wrap_char('{'), wrap_int(i), wrap_char('}'));
        }
      } else {
          if (i <= 9) {
            return string_concat(wrap_char('$'), wrap_int(i));
        } else {
            return string_concat3(wrap_str_lit("${"), wrap_int(i), wrap_char('}'));
        }
      }
    default:
      fatal_error("format_special_var: unknown identifier type");
      return 0;
  }
}

text global_var(int ident_probe) {
  return string_concat(wrap_char('_'), wrap_str_pool(ident_probe));
}

text local_var(int ident_probe) {
  if (ident_probe == ARGV_ID) {
    return wrap_str_lit("argv_");
  } else {
    return wrap_str_pool(ident_probe);
  }
}

text local_var_or_param(int ident_probe, int binding, bool prefixed_with_dollar) {
  if (binding_kind(binding) == BINDING_PARAM_LOCAL && is_constant_type(heap[binding + 4])) {
    if (prefixed_with_dollar) {
      return wrap_int(heap[binding + 3]);
    } else {
      return string_concat(wrap_char('$'), wrap_int(heap[binding + 3]));
    }
  } else {
    return local_var(ident_probe);
  }
}

text env_var_with_prefix(ast ident, bool prefixed_with_dollar) {
  int binding;
  int ident_probe;
  if (get_op(ident) == IDENTIFIER) {
    ident_probe = get_val_(IDENTIFIER, ident);
    if ((binding = cgc_lookup_var(ident_probe, cgc_locals))) {
      return local_var_or_param(ident_probe, binding, prefixed_with_dollar);
    } else {
      return global_var(ident_probe);
    }
  } else {
    return format_special_var(ident, prefixed_with_dollar);
  }
}

#ifdef SUPPORT_STRUCT_UNION

text struct_member_var(ast member_name_ident) {
  return string_concat(wrap_str_lit("__"), wrap_str_pool(get_val_(IDENTIFIER, member_name_ident)));
}

text struct_sizeof_var(ast struct_name_ident) {
  return string_concat(wrap_str_lit("__sizeof__"), wrap_str_pool(get_val_(IDENTIFIER, struct_name_ident)));
}

#endif

text function_name(int ident_tok) {
  return string_concat(wrap_char('_'), wrap_str_pool(ident_tok));
}

ast new_dollar_ident(int ix) {
  if (ix < IDENTIFIER_DOLLAR_PREALLOC_SIZE) {
    return preallocated_dollar_idents[ix];
  } else {
    return new_ast0(IDENTIFIER_DOLLAR, ix);
  }
}

ast new_fresh_ident(int ix) {
  if (ix < IDENTIFIER_INTERNAL_PREALLOC_SIZE) {
    return preallocated_fresh_idents[ix];
  } else {
    return new_ast0(IDENTIFIER_INTERNAL, ix);
  }
}

ast fresh_ident() {
  gensym_ix += 1;
  fun_gensym_ix = gensym_ix > fun_gensym_ix ? gensym_ix : fun_gensym_ix;
  max_gensym_ix = gensym_ix > max_gensym_ix ? gensym_ix : max_gensym_ix;

  return new_fresh_ident(gensym_ix);
}

ast fresh_string_ident(int string_probe) {
  // Strings are interned, meaning that the same string used twice will have the
  // same address. We use the token tag to mark the string as already defined.
  // This allows comp_defstr to use the same string variable for the same string.
  if (heap[string_probe + 3] == 0) { // tag defaults to 0
    string_counter += 1;
    heap[string_probe + 3] = string_counter - 1;
  }
  return new_ast0(IDENTIFIER_STRING, heap[string_probe + 3]);
}

void add_var_to_local_env(ast decl, enum BINDING kind) {
  int ident_probe = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, decl, 0));

  // Make sure we're not shadowing an existing local variable
  if (cgc_lookup_var(ident_probe, cgc_locals)) {
    dump_ident(ident_probe);
    fatal_error("Variable is already in local environment");
  }

  // The var is not part of the environment, so we add it.
  cgc_add_local_var(kind, ident_probe, get_child_(DECL, decl, 1));
}

// Since global and internal variables are prefixed with _, we restrict the name
// of variables to not start with _. Also, because some shells treat some
// variables as special, we prevent their use.
//
// Also, the shell backend doesn't support variables with aggregate types.
void assert_var_decl_is_safe(ast variable, bool local) { // Helper function for assert_idents_are_safe
  ast ident_probe = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, variable, 0));
  char* name = STRING_BUF(ident_probe);
  ast type = get_child_(DECL, variable, 1);
  if (name[0] == '_'
  || (name[0] != '\0' && name[1] == '_' && name[2] == '\0')) { // Check for a_ variables that could conflict with character constants
    dump_string("Variable name: ", name);
    fatal_error("variable name is invalid. It can't start or end with '_'.");
  }

  // IFS is a special shell variable that's overwritten by certain.
  // In zsh, writing to argv assigns to $@, so we map argv to argv_, and forbid argv_.
  // This check only applies to local variables because globals are prefixed with _.
  if (local && (ident_probe == ARGV__ID || ident_probe == IFS_ID)) {
    dump_string("Variable name: ", name);
    fatal_error("variable name is invalid. It can't be 'IFS' or 'argv_'.");
  }

  if (local) {
    // Local variables don't correspond to memory locations, and can't store
    // more than 1 number/pointer.
    if (get_op(type) == '['
#ifdef SUPPORT_STRUCT_UNION
    || get_op(type) == STRUCT_KW
#endif
       ) {
      dump_string("Variable name: ", name);
      fatal_error("array/struct value type is not supported for shell backend. Use a reference type instead.");
    }
  } else {
    // Arrays of structs and struct value types are not supported for now.
    // When we have type information on the local and global variables, we'll
    // be able to generate the correct code for these cases.
    if ( (get_op(type) == '[' && get_op(get_child_('[', type, 0)) == '[') // Array of arrays
#ifdef SUPPORT_STRUCT_UNION
      || (get_op(type) == '[' && get_op(get_child_('[', type, 0)) == STRUCT_KW) // Array of structs
      || get_op(type) == STRUCT_KW // Struct value type
#endif
       ) {
      dump_string("Variable name: ", name);
      fatal_error("array of struct and struct value type are not supported in shell backend. Use a reference type instead.");
    }
  }
}

void handle_function_params(ast lst) {
  while (lst != 0) {
    ast decl = car_(DECL, lst);
    assert_var_decl_is_safe(decl, true);
    add_var_to_local_env(decl, BINDING_PARAM_LOCAL);
    lst = tail(lst);
  }
}

#ifdef SH_SAVE_VARS_WITH_SET
// Save the value of local variables to positional parameters
text save_local_vars() {
  int env = cgc_locals_fun;
  ast ident;
  text res = 0;
  int counter = fun_gensym_ix;

  // Save local variables and parameters
  while (env != 0) {
    ident = binding_ident(env);
    if (binding_kind(env) != BINDING_PARAM_LOCAL || !is_constant_type(heap[env + 4])) { // Skip constant params
      res = concatenate_strings_with(string_concat(wrap_char('$'), local_var(ident)), res, wrap_char(' '));
    }
    env = binding_next(env);
  }

  // Save internal variables
  while (counter > 0) {
    res = concatenate_strings_with(res, string_concat(wrap_char('$'), INTERNAL_VAR_FORMAT(fun_gensym_ix - counter + 1)), wrap_char(' '));
    counter -= 1;
  }

  if (res) {
    runtime_use_local_vars = true;
    return string_concat(wrap_str_lit("set $@ "), res);
  } else {
    return 0;
  }
}

// Restore the previous value of local variables from positional parameters
text restore_local_vars(int params_count) {
  int env = cgc_locals_fun;
  ast ident;
  // Position of the saved local vars, starting from 0
  int local_var_pos = 0;
  text res = 0;
  int counter = fun_gensym_ix;
  // Number of non-constant variables in the environment.
  // Used to account for traversal of local env in reverse order.
  int env_non_cst_size = 0;

  while (env != 0) {
    if (binding_kind(env) != BINDING_PARAM_LOCAL || !is_constant_type(heap[env + 4])) { // Skip constant params
      env_non_cst_size += 1;
    }
    env = binding_next(env);
  }

  env = cgc_locals_fun;

  // Restore local variables and parameters
  while (env != 0) {
    ident = binding_ident(env);
    if (binding_kind(env) != BINDING_PARAM_LOCAL || !is_constant_type(heap[env + 4])) { // Skip constant params
      res = concatenate_strings_with(string_concat5(wrap_str_lit("$(("), local_var(ident), wrap_str_lit(" = $"), format_special_var(new_dollar_ident(params_count + env_non_cst_size - local_var_pos), true), wrap_str_lit("))")), res, wrap_char(' '));
      local_var_pos += 1;
    }
    env = binding_next(env);
  }

  // Restore internal variables
  while (counter > 0) {
    res = concatenate_strings_with(res, string_concat5(wrap_str_lit("$(("), INTERNAL_VAR_FORMAT(fun_gensym_ix - counter + 1), wrap_str_lit(" = $"), format_special_var(new_dollar_ident(params_count + local_var_pos + 1), true), wrap_str_lit("))")), wrap_char(' '));
    local_var_pos += 1;
    counter -= 1;
  }

  if (res) {
    runtime_use_local_vars = true;
    return string_concat3(wrap_str_lit(": $((__tmp = $1)) "), res, wrap_str_lit(" $(($1 = __tmp))"));
  } else {
    return 0;
  }
}

#else

#ifdef SH_INITIALIZE_PARAMS_WITH_LET
// Save the value of local variables to positional parameters
text let_params(int params) {
  ast ident, decl;
  text res = 0;
  int params_ix = 2;

  while (params != 0) {
    decl = car_(DECL, params);
    if (!is_constant_type(get_child_(DECL, decl, 1))) { // Skip constant params
      ident = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, decl, 0));
      res = concatenate_strings_with(res, string_concat4(wrap_str_lit("let "), local_var(ident), wrap_char(' '), format_special_var(new_dollar_ident(params_ix), false)), wrap_str_lit("; "));
    }
    params = tail(params);
    params_ix += 1;
  }

  runtime_use_local_vars |= res != 0;

  if (res != 0) res = string_concat(wrap_char(' '), res);

  return res;
}
#endif

text save_local_vars() {
  int env = cgc_locals_fun;
  ast ident;
  text res = 0;
  int counter = fun_gensym_ix;

  // Save temporary variables
  while (counter > 0) {
    ident = new_fresh_ident(counter);
    res = concatenate_strings_with(string_concat(wrap_str_lit("let "), format_special_var(ident, true)), res, wrap_str_lit("; "));
    counter -= 1;
  }

  // Save local variables and parameters
  while (env != 0) {
#if defined(SH_INITIALIZE_PARAMS_WITH_LET)
    if (binding_kind(env) != BINDING_PARAM_LOCAL) { // Skip params
#elif defined(OPTIMIZE_CONSTANT_PARAMS)
    if (binding_kind(env) != BINDING_PARAM_LOCAL || is_constant_type(heap[env + 4])) { // Skip constant params
#else
    {
#endif
      ident = binding_ident(env);
      res = concatenate_strings_with(string_concat(wrap_str_lit("let "), local_var(ident)), res, wrap_str_lit("; "));
    }

    env = binding_next(env);
  }

  runtime_use_local_vars |= res != 0;

  return res;
}

// The only difference between save_local_vars and restore_local_vars is the
// order of the arguments and the call to unsave_vars instead of save_vars.
text restore_local_vars(int params_count) {
  int env = cgc_locals_fun;
  ast ident;
  text res = 0;
  int counter = fun_gensym_ix;

  // Restore internal variables
  while (counter > 0) {
    ident = new_fresh_ident(counter);
    res = concatenate_strings_with(res, format_special_var(ident, false), wrap_char(' '));
    counter -= 1;
  }

  while (env != 0) {
    ident = binding_ident(env);
    if (binding_kind(env) != BINDING_PARAM_LOCAL || !is_constant_type(heap[env + 4])) { // Skip constant params
      res = concatenate_strings_with(res, local_var(ident), wrap_char(' '));
    }

    env = binding_next(env);
  }

  if (res) {
    runtime_use_local_vars = true;
    return string_concat(wrap_str_lit("endlet $1 "), res);
  } else {
    return 0;
  }
}
#endif

text op_to_str(int op) {
  if      (op < 256)         return string_concat3(wrap_char(' '), wrap_char(op), wrap_char(' '));
  else if (op == AMP_AMP)    return wrap_str_lit(" && ");
  else if (op == AMP_EQ)     return wrap_str_lit(" &= ");
  else if (op == BAR_BAR)    return wrap_str_lit(" || ");
  else if (op == BAR_EQ)     return wrap_str_lit(" |= ");
  else if (op == CARET_EQ)   return wrap_str_lit(" ^= ");
  else if (op == EQ_EQ)      return wrap_str_lit(" == ");
  else if (op == GT_EQ)      return wrap_str_lit(" >= ");
  else if (op == LSHIFT_EQ)  return wrap_str_lit(" <<= ");
  else if (op == LT_EQ)      return wrap_str_lit(" <= ");
  else if (op == LSHIFT)     return wrap_str_lit(" << ");
  else if (op == MINUS_EQ)   return wrap_str_lit(" -= ");
  else if (op == EXCL_EQ)    return wrap_str_lit(" != ");
  else if (op == PERCENT_EQ) return wrap_str_lit(" %= ");
  else if (op == PLUS_EQ)    return wrap_str_lit(" += ");
  else if (op == RSHIFT_EQ)  return wrap_str_lit(" >>= ");
  else if (op == RSHIFT)     return wrap_str_lit(" >> ");
  else if (op == SLASH_EQ)   return wrap_str_lit(" /= ");
  else if (op == STAR_EQ)    return wrap_str_lit(" *= ");
  else {
    dump_op(op);
    fatal_error("op_to_str: unexpected operator");
    return 0;
  }
}

// Similar to op_to_str, but returns the shell test operand instead of the C-style operands.
text test_op_to_str(int op) {
  // For == and !=, because integers are stored as strings in most shells, the
  // conversion to int can be avoided by comparing the strings instead of using
  // -eq and -ne.
  if      (op == EQ_EQ)      return wrap_str_lit(" = ");
  else if (op == EXCL_EQ)    return wrap_str_lit(" != ");
  else if (op == '<')        return wrap_str_lit(" -lt ");
  else if (op == '>')        return wrap_str_lit(" -gt ");
  else if (op == LT_EQ)      return wrap_str_lit(" -le ");
  else if (op == GT_EQ)      return wrap_str_lit(" -ge ");
  else {
    dump_op(op);
    fatal_error("test_op_to_str: unexpected operator");
    return 0;
  }
}

text character_ident(int c) {
  // Mark character as used
  characters_useds[c / CHARACTERS_BITFIELD_SIZE] |= 1 << (c % CHARACTERS_BITFIELD_SIZE);
  any_character_used = true;

  if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || ('0' <= c && c <= '9')) {
    return string_concat5(wrap_char('_'), wrap_char('_'), wrap_char(c), wrap_char('_'), wrap_char('_'));
  } else if (c < 32) {
    // First 32 characters are control characters.
    char* control_character_identifiers =
      "__NUL__\0"     "__SOH__\0"     "__STX__\0"     "__ETX__\0"
      "__EOT__\0"     "__ENQ__\0"     "__ACK__\0"     "__BEL__\0"
      "__BS__\0\0"    "__HT__\0\0"    "__LF__\0\0"    "__VT__\0\0"
      "__FF__\0\0"    "__CR__\0\0"    "__SO__\0\0"    "__SI__\0\0"
      "__DLE__\0"     "__DC1__\0"     "__DC2__\0"     "__DC3__\0"
      "__DC4__\0"     "__NAK__\0"     "__SYN__\0"     "__ETB__\0"
      "__CAN__\0"     "__EM__\0\0"    "__SUB__\0"     "__ESC__\0"
      "__FS__\0\0"    "__GS__\0\0"    "__RS__\0\0"    "__US__\0\0"
    ;
    // Control characters
    return wrap_str_lit(control_character_identifiers + (c * 8)); // Each string has length 7 + null terminator
  } else if (c < '0') {
    // Symbols from space to /
    char* printable_character_identifiers1 =
      "__SPACE__\0\0\0\0\0\0"     "__EXCL__\0\0\0\0\0\0\0"    "__DQUOTE__\0\0\0\0\0"        "__HASH__\0\0\0\0\0\0\0"
      "__DOLLAR__\0\0\0\0\0"      "__PERCENT__\0\0\0\0"       "__AMP__\0\0\0\0\0\0\0\0"     "__QUOTE__\0\0\0\0\0\0"
      "__LPAREN__\0\0\0\0\0"      "__RPAREN__\0\0\0\0\0"      "__STAR__\0\0\0\0\0\0\0"      "__PLUS__\0\0\0\0\0\0\0"
      "__COMMA__\0\0\0\0\0\0"     "__MINUS__\0\0\0\0\0\0"     "__PERIOD__\0\0\0\0\0"        "__SLASH__\0\0\0\0\0\0"
    ;
    return wrap_str_lit(printable_character_identifiers1 + (c - ' ') * 15); // Each string has length 14 + null terminator
  } else if (c <= 'A') {
    // Symbols from : to @
    char* printable_character_identifiers2 =
      "__COLON__\0\0\0\0\0\0"     "__SEMICOLON__\0\0"         "__LT__\0\0\0\0\0\0\0\0\0"    "__EQ__\0\0\0\0\0\0\0\0\0"
      "__GT__\0\0\0\0\0\0\0\0\0"  "__QUESTION__\0\0\0"        "__AT__\0\0\0\0\0\0\0\0\0"
    ;
    return wrap_str_lit(printable_character_identifiers2 + (c - ':') * 15); // Each string has length 14 + null terminator
  } else if (c <= 'a') {
    // Symbols from [ to `
    char* printable_character_identifiers3 =
      "__LBRACK__\0\0\0\0\0"      "__BACKSLASH__\0\0"         "__RBRACK__\0\0\0\0\0"        "__CARET__\0\0\0\0\0\0"
      "__UNDERSCORE__\0"          "__BACKTICK__\0\0\0"
    ;
    return wrap_str_lit(printable_character_identifiers3 + (c - '[') * 15); // Each string has length 14 + null terminator
  } else if (c <= 127) {
    // Symbols from { to ~ and DEL (127)
    char* printable_character_identifiers4 =
      "__LBRACE__\0\0\0\0\0"      "__BAR__\0\0\0\0\0\0\0\0"   "__RBRACE__\0\0\0\0\0"        "__TILDE__\0\0\0\0\0\0"
      "__DEL__\0\0\0\0\0\0\0\0"
    ;
    return wrap_str_lit(printable_character_identifiers4 + (c - '{') * 15); // Each string has length 14 + null terminator
  } else {
    dump_char(c);
    fatal_error("character_ident: invalid character");
    return 0;
  }
}

ast replaced_fun_calls = 0;
ast replaced_fun_calls_tail = 0;
ast conditional_fun_calls = 0;
ast conditional_fun_calls_tail = 0;
ast literals_inits = 0;
bool contains_side_effects = 0;

ast handle_fun_call_side_effect(ast node, ast assign_to, bool executes_conditionally) {
  int start_gensym_ix = gensym_ix;
  ast sub1;

  if (assign_to == 0) {
    assign_to = fresh_ident(); // Unique identifier for the function call
    start_gensym_ix = gensym_ix;

    // At this point, the temporary identifier of the variable is not live and
    // can be used to evaluate the function arguments. This reduces the number
    // of temporary variables.
    gensym_ix -= 1;
  }

  // Traverse the arguments and replace them with the result of
  // handle_side_effects_go sub is the parent node of the current argument
  sub1 = get_child_('(', node, 1);
  while (sub1 != 0) {
    set_child(sub1, 0, handle_side_effects_go(car(sub1), executes_conditionally));
    sub1 = tail(sub1);
  }

  // All the temporary variables used for the function parameters can be
  // reused after the function call, so resetting the gensym counter.
  gensym_ix = start_gensym_ix;

  sub1 = cons(new_ast2('=', assign_to, node), 0);
  if (executes_conditionally) {
    if (conditional_fun_calls == 0) { conditional_fun_calls = sub1; }
    else { set_child(conditional_fun_calls_tail, 1, sub1); }
    conditional_fun_calls_tail = sub1;
  }
  else {
    if (replaced_fun_calls == 0) { replaced_fun_calls = sub1; }
    else { set_child(replaced_fun_calls_tail, 1, sub1); }
    replaced_fun_calls_tail = sub1;
  }

  return assign_to;
}

// We can't have function calls and other side effects in $(( ... )), so we need to handle them separately.
// For unconditional function calls, they are replaced with unique identifiers and returned as a list with their new identifiers.
// For pre/post-increments/decrements, we map them to a pre-side-effects and replace with the corresponding operation.
// Note that pre/post-increments/decrements of function calls are not supported.
ast handle_side_effects_go(ast node, bool executes_conditionally) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  ast sub1, sub2;
  ast previous_conditional_fun_calls;
  ast left_conditional_fun_calls, right_conditional_fun_calls;
  int start_gensym_ix = gensym_ix;
  ast child0, child1, child2;

  if (nb_children >= 1) { child0 = get_child(node, 0); }
  if (nb_children >= 2) { child1 = get_child(node, 1); }
  if (nb_children >= 3) { child2 = get_child(node, 2); }

  if (nb_children == 0) {
    if ( op == IDENTIFIER || op == IDENTIFIER_INTERNAL || op == IDENTIFIER_STRING || op == IDENTIFIER_DOLLAR
      || op == CHARACTER  || op == INTEGER
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
      || op == INTEGER_HEX || op == INTEGER_OCT
#endif
      ) {
      return node;
    } else if (op == STRING) {
      /* We must initialize strings before the expression */
      sub1 = fresh_string_ident(get_val_(STRING, node));
      literals_inits = cons(new_ast2('=', sub1, get_val_(STRING, node)), literals_inits);
      return sub1;
    } else {
      dump_node(node);
      fatal_error("unexpected operator");
      return 0;
    }
  } else if (nb_children == 1) {
    if (op == '&' || op == '*' || op == '+' || op == '-' || op == '~' || op == '!') {
      // TODO: Reuse ast node?
      return new_ast1(op, handle_side_effects_go(child0, executes_conditionally));
    } else if (op == PLUS_PLUS_PRE || op == MINUS_MINUS_PRE || op == PLUS_PLUS_POST || op == MINUS_MINUS_POST) {
      contains_side_effects = true;
      return new_ast1(op, handle_side_effects_go(child0, executes_conditionally));
    } else if (op == SIZEOF_KW) {
      return node; // sizeof is a compile-time operator
    } else {
      dump_node(node);
      fatal_error("unexpected operator");
      return 0;
    }
  } else if (nb_children == 2) {
    if (op == '(') { // Function call
      return handle_fun_call_side_effect(node, 0, executes_conditionally);
    } else if (op == '=') {
      if (get_op(child1) == '(') { // Function call
        // In that case, we reuse the left hand side of the assignment as the result location
        return handle_fun_call_side_effect(child1, child0, executes_conditionally);
      } else {
        sub1 = handle_side_effects_go(child0, executes_conditionally);
        sub2 = handle_side_effects_go(child1, executes_conditionally); // We could inline that one since the assignment to the global variable is done after the last handle_side_effects_go call
        return new_ast2(op, sub1, sub2);
      }
    } else if (op == '&' || op == '|' || op == '<' || op == '>' || op == '+' || op == '-' || op == '*' || op == '/'
      || op == '%' || op == '^' || op == ',' || op == EQ_EQ || op == EXCL_EQ || op == LT_EQ || op == GT_EQ || op == LSHIFT || op == RSHIFT || op == '['
#ifdef SUPPORT_STRUCT_UNION
      || op == '.' || op == ARROW
    #endif
       ) {
      sub1 = handle_side_effects_go(child0, executes_conditionally);
      sub2 = handle_side_effects_go(child1, executes_conditionally); // We could inline that one since the assignment to the global variable is done after the last handle_side_effects_go call
      return new_ast2(op, sub1, sub2);
    } else if (op == AMP_EQ || op == BAR_EQ || op == CARET_EQ || op == LSHIFT_EQ || op == MINUS_EQ || op == PERCENT_EQ || op == PLUS_EQ || op == RSHIFT_EQ || op == SLASH_EQ || op == STAR_EQ) {
      // Just like previous case, except that we update contains_side_effects
      contains_side_effects = true;
      sub1 = handle_side_effects_go(child0, executes_conditionally);
      sub2 = handle_side_effects_go(child1, executes_conditionally); // We could inline that one since the assignment to the global variable is done after the last handle_side_effects_go call
      return new_ast2(op, sub1, sub2);
    } else if (op == AMP_AMP || op == BAR_BAR) {
      previous_conditional_fun_calls = conditional_fun_calls;
      conditional_fun_calls = 0;
      // The left side is always executed, unless the whole expression is executed conditionally.
      // We could compile it as always executed, but it makes the Shell code less regular so we compile it conditionally.
      sub1 = handle_side_effects_go(child0, true);
      gensym_ix = start_gensym_ix; // Reset gensym counter because the 2 sides are independent
      left_conditional_fun_calls = conditional_fun_calls;
      conditional_fun_calls = 0;
      sub2 = handle_side_effects_go(child1, true);
      gensym_ix = start_gensym_ix; // Reset gensym counter because the 2 sides are independent
      right_conditional_fun_calls = conditional_fun_calls;
      conditional_fun_calls = previous_conditional_fun_calls;
      return new_ast4(op, sub1, sub2, left_conditional_fun_calls, right_conditional_fun_calls);
    } else if (op == CAST) {
      return new_ast2(CAST, child0, handle_side_effects_go(child1, executes_conditionally));
    } else {
      fatal_error("unexpected operator");
      return 0;
    }
  } else if (nb_children == 3 && op == '?') {
    previous_conditional_fun_calls = conditional_fun_calls;
    conditional_fun_calls = 0;
    sub1 = handle_side_effects_go(child1, true);
    left_conditional_fun_calls = conditional_fun_calls;
    conditional_fun_calls = 0;
    sub2 = handle_side_effects_go(child2, true);
    right_conditional_fun_calls = conditional_fun_calls;
    if (left_conditional_fun_calls != 0 || right_conditional_fun_calls != 0) {
      fatal_error("Conditional function calls in ternary operator not allowed");
    }

    return new_ast3('?', handle_side_effects_go(child0, executes_conditionally), sub1, sub2);
  } else {
    dump_node(node);
    fatal_error("unexpected operator");
    return 0;
  }
}

ast handle_side_effects(ast node) {
  replaced_fun_calls = 0;
  conditional_fun_calls = 0;
  literals_inits = 0;
  contains_side_effects = false;
  return handle_side_effects_go(node, false);
}

void comp_defstr(ast ident, int string_probe, int array_size) {
  char *string_start = STRING_BUF(string_probe);
  char *string_end = STRING_BUF_END(string_probe);
  text array_size_text = 0;

  if (array_size != -1) {
    array_size_text = string_concat(wrap_char(' '), wrap_int(array_size));
  }

  if (top_level_stmt) {
    // If defstr is used at the top level, it needs to be included beforehand
    runtime_defstr();
  } else {
    runtime_use_defstr = true;
  }

  append_glo_decl(string_concat4( wrap_str_lit("defstr ")
                                , env_var_with_prefix(ident, false)
                                , string_concat3( wrap_str_lit(" \"")
                                                , escape_text(wrap_str_imm(string_start, string_end), false)
                                                , wrap_char('\"')
                                                )
                                , array_size_text));
}

#ifdef SUPPORT_COMPLEX_INITIALIZER

int initializer_list_len(ast node) {
  int res = 0;

  // Each element of the list has size 1 since nested initializers are not allowed
  while (node != 0) {
    res += 1;
    node = tail(node);
  }

  return res;
}

text comp_initializer_list(ast initializer_list, int expected_len) {
  text args = 0;
  ast element;
  ast str_ident;

  runtime_use_initialize = true;

  while (initializer_list != 0) {
    element = car(initializer_list);
    switch (get_op(element)) {
      case INTEGER:
        args = concatenate_strings_with(args, wrap_int(-get_val_(INTEGER, element)), wrap_char(' '));
        break;
#ifndef PARSE_NUMERIC_LITERAL_WITH_BASE
      case INTEGER_HEX:
      case INTEGER_OCT:
        // We need to wrap in $(( ... )) to make sure the number is converted to base 10 when stored in a variable.
        args = concatenate_strings_with(args, string_concat3(wrap_str_lit("$(("), wrap_integer(1, element), wrap_str_lit("))")), wrap_char(' '));
        break;
#endif
      case CHARACTER:
        // TODO: Character identifiers are only defined at the end of the script, so we can't use them here
        args = concatenate_strings_with(args, wrap_int(get_val_(CHARACTER, element)), wrap_char(' '));
        break;
      case STRING:
        str_ident = fresh_string_ident(get_val_(STRING, element));
        comp_defstr(str_ident, get_val_(STRING, element), -1);
        args = concatenate_strings_with(args, string_concat(wrap_char('$'), format_special_var(str_ident, true)), wrap_char(' '));
        break;
      default:
        // TODO: Support nested initializers and constant expressions
        fatal_error("comp_initializer: unexpected operator");
    }
    initializer_list = tail(initializer_list);
  }

  return args;
}

#endif // SUPPORT_COMPLEX_INITIALIZER

enum VALUE_CTX {
  RVALUE_CTX_BASE,            // value is outside of $(( ... ))
  RVALUE_CTX_ARITH_EXPANSION, // value is already in $(( ... ))
  RVALUE_CTX_TEST,            // value is inside of a test
  RVALUE_CTX_TEST_ELSEIF      // value is inside of a elseif test
};

text with_prefixed_side_effects(ast test_side_effects, text code) {
  text test_side_effects_code = 0;
  ast side_effect;

  while (test_side_effects != 0) {
    side_effect = car_('=', test_side_effects);
    test_side_effects_code =
      string_concat3(test_side_effects_code,
                     comp_fun_call_code(get_child_('=', side_effect, 1), get_child_('=', side_effect, 0)),
                     wrap_str_lit("; "));
    test_side_effects = tail(test_side_effects);
  }
  if (test_side_effects_code != 0) {
    return string_concat4(wrap_str_lit("{ "), test_side_effects_code, code, wrap_str_lit("; }"));
  } else {
    return code;
  }
}

// Return true if the operator is associative.
// Associative operators can be chained without parentheses.
bool is_associative_operator(int op) {
  return (op == '+')   | (op == '*')     | (op == '&')    | (op == '|')    | (op == '^')
      |  (op == EQ_EQ) | (op == AMP_AMP) | (op == BAR_BAR);
}

// Wrap code in $((...)) if it's not already and if it's already in $(( )), wrap
// it in parentheses if parens_otherwise is true. If it's not in an arithmetic
// expansion and we're compiling tests, we also add the test condition to make it
// a valid test.
text wrap_if_needed(int parens_otherwise, int context, ast test_side_effects, text code, int outer_op, int inner_op) {
  // Rough heuristic to determine if we need to wrap in parentheses. If we
  // wanted to do this right, we'd track the left and right operators and
  // use this information to determine if parentheses are needed.
  if (context == RVALUE_CTX_ARITH_EXPANSION) {
    if ( parens_otherwise
      && outer_op != 0
      && outer_op != '=' // Assignment has the lowest precedence so we never use parentheses
      && (!is_associative_operator(inner_op) || inner_op != outer_op) // Adjacent associative operations don't need parentheses
      ) {
      return string_concat3(wrap_char('('), code, wrap_char(')'));
    }
    else return code;
  } else if (context == RVALUE_CTX_TEST) {
    return with_prefixed_side_effects(test_side_effects, string_concat3(wrap_str_lit("[ $(("), code, wrap_str_lit(")) != 0 ]")));
  } else {
    return string_concat3(wrap_str_lit("$(("), code, wrap_str_lit("))"));
  }
}

// Used to supports the case `if/while (c) { ... }`, where c is a variable or a literal.
// This is otherwise handled by wrap-if-needed, but we don't want to wrap in $(( ... )) here.
text wrap_in_condition_if_needed(int context, ast test_side_effects, text code) {
  if (context == RVALUE_CTX_TEST) {
    return with_prefixed_side_effects(test_side_effects, string_concat3(wrap_str_lit("[ "), code, wrap_str_lit(" != 0 ]")));
  } else {
    return code;
  }
}

text comp_rvalue_go(ast node, int context, ast test_side_effects, int outer_op) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  text sub1, sub2, sub3;
  ast child0, child1, child2, child3;

  if (nb_children >= 1) { child0 = get_child(node, 0); }
  if (nb_children >= 2) { child1 = get_child(node, 1); }
  if (nb_children >= 3) { child2 = get_child(node, 2); }
  if (nb_children >= 4) { child3 = get_child(node, 3); }

  if (nb_children == 0) {
    if (op == INTEGER) {
      return wrap_in_condition_if_needed(context, test_side_effects, wrap_int(-get_val_(INTEGER, node)));
    }
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
    else if (op == INTEGER_HEX || op == INTEGER_OCT) {
      // We need to wrap in $(( ... )) to make sure the number is converted to base 10 when stored in a variable.
      return wrap_if_needed(false, context, test_side_effects, wrap_integer(1, node), outer_op, op);
    }
#endif
    else if (op == CHARACTER) {
#ifdef SH_INLINE_CHAR_LITERAL
      return wrap_in_condition_if_needed(context, test_side_effects, wrap_int(get_val_(CHARACTER, node)));
#else
      if (context == RVALUE_CTX_ARITH_EXPANSION) {
        return character_ident(get_val_(CHARACTER, node));
      } else {
        return wrap_in_condition_if_needed(context, test_side_effects, string_concat(wrap_char('$'), character_ident(get_val_(CHARACTER, node))));
      }
#endif
    } else if (op == IDENTIFIER || op == IDENTIFIER_INTERNAL || op == IDENTIFIER_STRING || op == IDENTIFIER_DOLLAR) {
      if (context == RVALUE_CTX_ARITH_EXPANSION) { return env_var_with_prefix(node, false); }
      else { return wrap_in_condition_if_needed(context, test_side_effects, string_concat(wrap_char('$'), env_var_with_prefix(node, true))); }
    } else {
      dump_node(node);
      fatal_error("comp_rvalue_go: unexpected operator");
      return 0;
    }
  } else if (nb_children == 1) {
    if (op == '*') {
      // Setting context to RVALUE_CTX_BASE even if it's wrapped in $(( ... )) because we
      // need another layer of wrapping if it's a complex expression, i.e. not a
      // literal or a variable.
      sub1 = comp_rvalue_go(child0, RVALUE_CTX_BASE, 0, op);
      return wrap_if_needed(false, context, test_side_effects, string_concat(wrap_char('_'), sub1), outer_op, op);
    } else if (op == '+') {
      // +x is equivalent to x
      return comp_rvalue_go(child0, context, test_side_effects, outer_op);
    } else if (op == '-' || op == '~' || op == '!') {
      if (op == '-' && (get_op(child0) == INTEGER
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
       || op == INTEGER_HEX || op == INTEGER_OCT
#endif
      )
      ) {
        return wrap_in_condition_if_needed(context, test_side_effects, wrap_integer(-1, child0));
      }
#ifdef OPTIMIZE_CONSTANT_PARAMS
      // The expansion of negative constant params prefixed with - result in
      // --<number> which is parsed as pre-decrement. Add a space between the
      // operator and the variable (constant param or not for consistency).
      else if (get_op(child0) == IDENTIFIER) {
        return wrap_if_needed(false, context, test_side_effects, string_concat3(wrap_char(op), wrap_char(' '), env_var_with_prefix(child0, false)), outer_op, op);
      }
#endif
      else {
        sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, op);
        return wrap_if_needed(false, context, test_side_effects, string_concat(wrap_char(op), sub1), outer_op, op);
      }
    } else if (op == MINUS_MINUS_PRE) {
      sub1 = comp_lvalue(child0);
      return wrap_if_needed(true, context, test_side_effects, string_concat(sub1, wrap_str_lit(" -= 1")), outer_op, op);
    } else if (op == PLUS_PLUS_PRE) {
      sub1 = comp_lvalue(child0);
      return wrap_if_needed(true, context, test_side_effects, string_concat(sub1, wrap_str_lit(" += 1")), outer_op, op);
    } else if (op == MINUS_MINUS_POST) {
      sub1 = comp_lvalue(child0);
      return wrap_if_needed(false, context, test_side_effects,string_concat4(wrap_char('('), sub1, wrap_str_lit(" -= 1)"), wrap_str_lit(" + 1")), outer_op, '+');
    } else if (op == PLUS_PLUS_POST) {
      sub1 = comp_lvalue(child0);
      return wrap_if_needed(false, context, test_side_effects, string_concat4(wrap_char('('), sub1, wrap_str_lit(" += 1)"), wrap_str_lit(" - 1")), outer_op, '-');
    } else if (op == SIZEOF_KW) {
      // child0 is either an abstract declaration or an expression
      if (get_op(child0) == DECL) {
        child0 = get_child_(DECL, child0, 1); // Get the type
        switch (get_op(child0)) {
          case INT_KW:
          case SHORT_KW:
          case LONG_KW:
          case CHAR_KW:
          case VOID_KW:
          case ENUM_KW:
          case '*': // If it's a pointer
            return wrap_in_condition_if_needed(context, test_side_effects, wrap_int(1));

#ifdef SUPPORT_STRUCT_UNION
          case STRUCT_KW:
            return wrap_if_needed(false, context, test_side_effects, struct_sizeof_var(get_child__(STRUCT_KW, IDENTIFIER, child0, 1)), outer_op, op);
#endif
          default:
            dump_node(child0);
            dump_node(get_child(child0, 1));
            fatal_error("comp_rvalue_go: sizeof is not supported for this type or expression");
            return 0;
        }
      } else {
        dump_node(child0);
        fatal_error("comp_rvalue_go: sizeof is not supported for this type or expression");
        return 0;
      }
    } else if (op == '&') {
      return wrap_if_needed(false, context, test_side_effects, comp_lvalue_address(child0), outer_op, op);
    } else {
      dump_node(node);
      fatal_error("comp_rvalue_go: unexpected operator");
      return 0;
    }
  } else if (nb_children == 2) {
    if (op == '+' || op == '-' || op == '*' || op == '/' || op == '%' || op == '&' || op == '|' || op == '^' || op == LSHIFT || op == RSHIFT || op == ',') {
      sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      sub2 = comp_rvalue_go(child1, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      return wrap_if_needed(true, context, test_side_effects, string_concat3(sub1, op_to_str(op), sub2), outer_op, op);
    } else if (op == '=' || op == AMP_EQ || op == BAR_EQ || op == CARET_EQ || op == LSHIFT_EQ || op == MINUS_EQ || op == PERCENT_EQ || op == PLUS_EQ || op == RSHIFT_EQ || op == SLASH_EQ || op == STAR_EQ) {
      sub1 = comp_lvalue(child0);
      sub2 = comp_rvalue_go(child1, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      return wrap_if_needed(true, context, test_side_effects, string_concat3(sub1, op_to_str(op), sub2), outer_op, op);
    } else if (op == '[') { // array indexing
      sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, '+');
      sub2 = comp_rvalue_go(child1, RVALUE_CTX_ARITH_EXPANSION, 0, '+');
      return wrap_if_needed(false, context, test_side_effects, string_concat5(wrap_str_lit("_$(("), sub1, wrap_str_lit(" + "), sub2, wrap_str_lit("))")), outer_op, op);
    }
#ifdef SUPPORT_STRUCT_UNION
    else if (op == ARROW) { // member access is implemented like array access
      sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      sub2 = struct_member_var(child1);
      return wrap_if_needed(false, context, test_side_effects, string_concat5(wrap_str_lit("_$(("), sub1, wrap_str_lit(" + "), sub2, wrap_str_lit("))")), outer_op, op);
    }
#endif
    else if (op == EQ_EQ || op == EXCL_EQ || op == LT_EQ || op == GT_EQ || op == '<' || op == '>') {
      if (context == RVALUE_CTX_TEST) {
        sub1 = comp_rvalue_go(child0, RVALUE_CTX_BASE, 0, op);
        sub2 = comp_rvalue_go(child1, RVALUE_CTX_BASE, 0, op);
        return with_prefixed_side_effects(test_side_effects, string_concat5(wrap_str_lit("[ "), sub1, test_op_to_str(op), sub2, wrap_str_lit(" ]")));
      } else {
        sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, op);
        sub2 = comp_rvalue_go(child1, RVALUE_CTX_ARITH_EXPANSION, 0, op);
        return wrap_if_needed(true, context, test_side_effects, string_concat3(sub1, op_to_str(op), sub2), outer_op, op);
      }
    } else if (op == CAST) { // Casts are no-op
      return comp_rvalue_go(child1, context, 0, op);
    } else {
      fatal_error("comp_rvalue_go: unknown rvalue");
      return 0;
    }
  } else if (nb_children == 3 && op == '?') {
    sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, op);
    sub2 = comp_rvalue_go(child1, RVALUE_CTX_ARITH_EXPANSION, 0, op);
    sub3 = comp_rvalue_go(child2, RVALUE_CTX_ARITH_EXPANSION, 0, op);
    return wrap_if_needed(true, context, test_side_effects, string_concat5(sub1, op_to_str(op), sub2, wrap_str_lit(": "), sub3), outer_op, op);
    return 0;
  } else if (nb_children == 4 && (op == AMP_AMP || op == BAR_BAR)) {
    // Note, this could also be compiled in a single [ ] block using -a and
    // -o, which I think are POSIX compliant but are deprecated.
    if (context == RVALUE_CTX_TEST) {
      // When compiling in a test context, && and || can be compiled to
      // Shell's && and || with [ ... ] blocks.
      //
      // A notable difference between these operators in Shell and C is that
      // in Shell, they have equal precedence while in C, && has higher
      // precedence. This means that we need to add parenthesis that would not
      // be needed in C.
      //
      // As a heuristic, we add parenthesis whenever the left or right side of
      // the operator is a different comparison operator.

      // if lhs is && or ||, and different from the current operator
      if ((get_op(child0) == AMP_AMP || get_op(child0) == BAR_BAR) && get_op(child0) != op) {
        sub1 = comp_rvalue_go(child0, RVALUE_CTX_TEST, child2, op);
        sub1 = string_concat3(wrap_str_lit("{ "), sub1, wrap_str_lit("; }"));
      } else {
        sub1 = comp_rvalue_go(child0, RVALUE_CTX_TEST, child2, op);
      }

      // if rhs is && or ||, and different from the current operator
      if ((get_op(child1) == AMP_AMP || get_op(child1) == BAR_BAR) && get_op(child1) != op) {
        sub2 = comp_rvalue_go(child1, RVALUE_CTX_TEST, child3, op);
        sub2 = string_concat3(wrap_str_lit("{ "), sub2, wrap_str_lit("; }"));
      } else {
        sub2 = comp_rvalue_go(child1, RVALUE_CTX_TEST, child3, op);
      }
      return string_concat3(sub1, op_to_str(op), sub2);
    } else {
      if (test_side_effects != 0 || child2 != 0 || child3 != 0) {
        fatal_error("comp_rvalue_go: && and || with function calls can only be used in tests");
      }
      sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      sub2 = comp_rvalue_go(child1, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      return wrap_if_needed(true, context, test_side_effects, string_concat3(sub1, op_to_str(op), sub2), outer_op, op);
    }
  } else {
    dump_node(node);
    fatal_error("comp_rvalue_go: unknown rvalue");
    return 0;
  }
}

text comp_rvalue(ast node, int context) {
  ast simple_ast = handle_side_effects(node);
  // Calling comp_fun_call/comp_rvalue can overwrite replaced_fun_calls and contains_side_effects, so they are saved
  ast replaced_fun_calls2 = replaced_fun_calls;
  int contains_side_effects2 = contains_side_effects;
  int fun_call_decl_start;
  text result;
  ast side_effect;

  // Capture the start of the side effects to be able to undo them if needed
  fun_call_decl_start = glo_decl_ix;

  while (literals_inits != 0) {
    side_effect = car_('=', literals_inits);
    comp_defstr(get_child_('=', side_effect, 0), get_child_('=', side_effect, 1), -1);
    literals_inits = tail(literals_inits);
  }

  // We don't want to call defstr on every iteration, so we only capture fun
  // calls, not literal initialization. That's unless it's for a elif statement,
  // because the previous block is not executed so the def_str calls must be
  // placed inline with the rest of the condition.
  if (context != RVALUE_CTX_TEST_ELSEIF)
    fun_call_decl_start = glo_decl_ix;

  while (replaced_fun_calls2 != 0) {
    side_effect = car_('=', replaced_fun_calls2);
    comp_fun_call(get_child_('=', side_effect, 1), get_child_('=', side_effect, 0));
    replaced_fun_calls2 = tail(replaced_fun_calls2);
  }

  // When compiling a test, we place the function side effects inline with the condition.
  // That way, any side effect performed in the condition of a while loop is repeated on each iteration.
  // For if statements, it makes things shorter, but not always more readable.
  if (context == RVALUE_CTX_TEST || context == RVALUE_CTX_TEST_ELSEIF) {
    undo_glo_decls(fun_call_decl_start);
    result = replay_glo_decls_inline(fun_call_decl_start, glo_decl_ix);
    result = string_concat(result, comp_rvalue_go(simple_ast, RVALUE_CTX_TEST, 0, 0));
  } else {
    result = comp_rvalue_go(simple_ast, context, 0, 0);
  }
  contains_side_effects |= contains_side_effects2;
  return result;
}

// Unlike in the native backend, there are 2 ways to compile a lvalue.
//
// The first (comp_lvalue) returns the variable that represent the memory
// location, this is useful when we're assigning to the lvalue.
// The second (comp_lvalue_address) produces the address of the memory location.
// This is mostly used to implement &.
//
// This difference is important as local variables don't have a memory location
// so we can't take their address and so their lvalue is just their name.
text comp_lvalue_address(ast node) {
  int op = get_op(node);
  text sub1;
  text sub2;

  if (op == IDENTIFIER) {
    // TODO: Support global variables when SUPPORT_ADDRESS_OF_OP
    //
    // This is currently not supported because we treat as globals the enums
    // and other hardcoded constants which is not what we want.
    //
    // We need to integrate the bindings local used in the exe backend here so
    // we can know more about variables other than "it's local" and "it's not
    // local so it must be global".
    fatal_error("comp_rvalue_go: can't take the address of a local variable");
    return 0;
  } else if (op == '[') {
    sub1 = comp_rvalue(get_child_('[', node, 0), RVALUE_CTX_ARITH_EXPANSION);
    sub2 = comp_rvalue(get_child_('[', node, 1), RVALUE_CTX_ARITH_EXPANSION);
    return string_concat3(sub1, wrap_str_lit(" + "), sub2);
  } else if (op == '*') {
    return comp_rvalue(get_child_('*', node, 0), RVALUE_CTX_BASE);
  }
#ifdef SUPPORT_STRUCT_UNION
  else if (op == ARROW) {
    sub1 = comp_rvalue(get_child_(ARROW, node, 0), RVALUE_CTX_ARITH_EXPANSION);
    sub2 = struct_member_var(get_child_(ARROW, node, 1));
    return string_concat3(sub1, wrap_str_lit(" + "), sub2);
  }
#endif
  else if (op == CAST) {
    return comp_lvalue_address(get_child_(CAST, node, 1));
  } else {
    dump_node(node);
    fatal_error("comp_lvalue_address: unknown lvalue");
    return 0;
  }
}

text comp_lvalue(ast node) {
  int op = get_op(node);
  text sub1;
  text sub2;

  if (op == IDENTIFIER || op == IDENTIFIER_INTERNAL || op == IDENTIFIER_STRING || op == IDENTIFIER_DOLLAR) {
    return env_var_with_prefix(node, false);
  } else if (op == '[') {
    sub1 = comp_rvalue(get_child_('[', node, 0), RVALUE_CTX_ARITH_EXPANSION);
    sub2 = comp_rvalue(get_child_('[', node, 1), RVALUE_CTX_ARITH_EXPANSION);
    return string_concat5(wrap_str_lit("_$(("), sub1, wrap_str_lit(" + "), sub2, wrap_str_lit("))"));
  } else if (op == '*') {
    sub1 = comp_rvalue(get_child_('*', node, 0), RVALUE_CTX_BASE);
    return string_concat(wrap_char('_'), sub1);
  }
#ifdef SUPPORT_STRUCT_UNION
  else if (op == ARROW) {
    sub1 = comp_rvalue(get_child_(ARROW, node, 0), RVALUE_CTX_ARITH_EXPANSION);
    sub2 = struct_member_var(get_child_(ARROW, node, 1));
    return string_concat5(wrap_str_lit("_$(("), sub1, wrap_str_lit(" + "), sub2, wrap_str_lit("))"));
  }
#endif
  else if (op == CAST) {
    return comp_lvalue(get_child_(CAST, node, 1));
  } else {
    dump_node(node);
    fatal_error("comp_lvalue: unknown lvalue");
    return 0;
  }
}

text fun_call_params(ast params) {
  ast param;
  text code_params = 0;

  while (params != 0) {
    param = comp_rvalue(car(params), RVALUE_CTX_BASE);
    code_params = concatenate_strings_with(code_params, param, wrap_char(' '));
    params = tail(params);
  }

  return code_params;
}

#if defined(SH_INLINE_PUTCHAR) || defined(SH_AVOID_PRINTF_USE)
text comp_putchar_inline(ast param) {
  text res;
  ast ident;
  char c;

  if (get_op(param) == CHARACTER) {
    c = get_val_(CHARACTER, param);
    if ((c >= 32 && c <= 126) || c == '\n') { // Printable ASCII characters + newline
      return string_concat3(wrap_str_lit("printf \""), escape_text(wrap_char(c), true), wrap_char('\"'));
    }
  }

  res = comp_rvalue(param, RVALUE_CTX_ARITH_EXPANSION);

  if (contains_side_effects) {
    ident = fresh_ident();
    append_glo_decl(string_concat4(comp_lvalue(ident), wrap_str_lit("=$(("), res, wrap_str_lit("))")));
    res = comp_lvalue(ident);
  } else if (get_op(param) != IDENTIFIER) {
    res = string_concat3(wrap_char('('), res, wrap_char(')')); // Wrap in parentheses to avoid priority of operations issues
  }

  res =
    string_concat3(
      string_concat3(wrap_str_lit("$(("), res, wrap_str_lit("/64))")),
      string_concat3(wrap_str_lit("$(("), res, wrap_str_lit("/8%8))")),
      string_concat3(wrap_str_lit("$(("), res, wrap_str_lit("%8))")));

  return string_concat(wrap_str_lit("printf \\\\"), res);
}
#endif

#ifdef SH_AVOID_PRINTF_USE
// format_str is from the string pool so immutable
text printf_call(char *format_str, char *format_str_end, text params_text, bool escape) {
  if (format_str == format_str_end) {
    return 0;
  } else {
    // Some shells interpret leading - as options. In that case, we add -- in front of the format string.
    return string_concat3(wrap_str_lit(format_str[0] == '-' ? "printf -- \"" : "printf \""),
                          escape_text(wrap_str_imm(format_str, format_str_end), escape),
                          concatenate_strings_with(wrap_char('\"'), params_text, wrap_char(' '))
                          );
  }
}

enum PRINTF_STATE {
  PRINTF_STATE_FLAGS,
  PRINTF_STATE_WIDTH,
  PRINTF_STATE_PRECISION,
  PRINTF_STATE_SPECIFIER
};

// _printf pulls a lot of dependencies from the runtime. In most cases the
// format string is known at compile time, and we can avoid calling printf by
// using the shell's printf instead. This function generates a sequence of shell
// printf and put_pstr equivalent to the given printf call.
void handle_printf_call(char *format_str, ast params) {
  ast param = 0; // Next parameter, if any
  char *format_start = format_str;
  char *specifier_start;
  // compiled parameters to be passed to printf
  text params_text = 0, width_text = 0, precision_text = 0;

  bool mod = false;
  bool has_width = false;
  bool has_precision = false;

  enum PRINTF_STATE state = PRINTF_STATE_FLAGS;

  while (*format_str != '\0') {
    // Param is consumed, get the next one
    if (param == 0 && params != 0) {
      param = car(params);
      params = tail(params);
    }

    if (mod) {
      switch (*format_str) {
        case ' ': case '#': case '+': case '-': case '0': // Flags
          // Flags correspond to 0x20,0x23,0x2b,0x2d,0x30 which are spread over
          // 16 bits meaning we can easily convert char -> bit if we wanted to.
          if (state != PRINTF_STATE_FLAGS) fatal_error("printf: flags must come before width and precision");
          break;

        // Width or precision literal
        case '1': case '2': case '3':
        case '4': case '5': case '6':
        case '7': case '8': case '9':
          if (state != PRINTF_STATE_FLAGS && state != PRINTF_STATE_PRECISION) fatal_error("printf: width or precision already specified");
          while ('0' <= *format_str && *format_str <= '9') format_str += 1; // Skip the rest of the number
          has_width = state == PRINTF_STATE_FLAGS ? true : has_width;
          has_precision = state == PRINTF_STATE_PRECISION ? true : has_precision;
          state += 1;      // Move to the next state (PRINTF_STATE_FLAGS => PRINTF_STATE_WIDTH, PRINTF_STATE_PRECISION => PRINTF_STATE_SPECIFIER)
          format_str -= 1; // Reprocess non-numeric character
          break;

        // Precision
        case '.':
          if (state >= PRINTF_STATE_PRECISION) fatal_error("printf: precision already specified");
          state = PRINTF_STATE_PRECISION;
          break;

        case '*':
          if (param == 0) fatal_error("printf: not enough parameters");
          if (state == PRINTF_STATE_FLAGS) {
            width_text = comp_rvalue(param, RVALUE_CTX_BASE);
            has_width = true;
          } else if (state == PRINTF_STATE_PRECISION) {
            precision_text = comp_rvalue(param, RVALUE_CTX_BASE);
            has_precision = true;
          } else {
            fatal_error("printf: width or precision already specified");
          }
          param = 0;
          break;

        case '%':
          if (state != PRINTF_STATE_FLAGS) fatal_error("printf: cannot use flags, width or precision with %%");
          mod = false;
          break;

        // The following options are the same between the shell's printf and C's printf
        case 'l': case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
          if (*format_str == 'l') {
            while (*format_str == 'l') format_str += 1; // Skip the 'l' for long
            if (*format_str != 'd' && *format_str != 'i' && *format_str != 'o' && *format_str != 'u' && *format_str != 'x' && *format_str != 'X') {
              dump_string("format_str = ", specifier_start);
              fatal_error("printf: unsupported format specifier");
            }
          }

          if (param == 0) fatal_error("printf: not enough parameters");
          params_text = concatenate_strings_with(params_text, width_text, wrap_char(' '));     // Add width param if needed
          params_text = concatenate_strings_with(params_text, precision_text, wrap_char(' ')); // Add precision param if needed
          params_text = concatenate_strings_with(params_text, comp_rvalue(param, RVALUE_CTX_BASE), wrap_char(' ')); // Add the parameter
          param = 0; // Consume param
          mod = false;
          break;

        // We can't pass characters to printf directly because %c has a different meaning.
        // We could use the %b format specifier, but it can't emit \0 character on some older
        // shells, so we make a separate printf call using the \\ooo format.
        // %c does not support the width parameter as it's not worth the extra complexity to handle * and numbers.
        case 'c':
          if (param == 0) fatal_error("printf: not enough parameters");
          // TODO: Find way to support width that's not too verbose
          if (has_width) fatal_error("printf: width not supported for %c");
          // Generate printf call with what we have so far
          append_glo_decl(printf_call(format_start, specifier_start, params_text, false));
          // New format string starts after the %
          format_start = format_str + 1;
          // Generate the printf call for the character
          append_glo_decl(comp_putchar_inline(param));
          param = 0; // Consume param
          params_text = 0; // Reset the parameters
          mod = false;
          break;

        // We can't a string to printf directly, it needs to be unpacked first.
        case 's':
          if (param == 0) fatal_error("printf: not enough parameters");
          runtime_use_put_pstr = true;
          // If the format specifier has width or precision, we have to pack the string and call then printf.
          // Otherwise, we can call _put_pstr directly and avoid the subshell.
          if (has_width || has_precision) {
            params_text = concatenate_strings_with(params_text, width_text, wrap_char(' '));     // Add width param if needed
            params_text = concatenate_strings_with(params_text, precision_text, wrap_char(' ')); // Add precision param if needed
            params_text = concatenate_strings_with(params_text, string_concat3(wrap_str_lit("\"$(_put_pstr __ "), comp_rvalue(param, RVALUE_CTX_BASE), wrap_str_lit(")\"")), wrap_char(' ')); // Add the parameter
          } else {
            // Generate printf call with what we have so far
            append_glo_decl(printf_call(format_start, specifier_start, params_text, false));
            // New format string starts after the %
            format_start = format_str + 1;
            // Compile printf("...%s...", str) to _put_pstr str
            append_glo_decl(string_concat(wrap_str_lit("_put_pstr __ "), comp_rvalue(param, RVALUE_CTX_BASE)));
          }
          param = 0; // Consume param
          mod = false;
          break;

        default:
          dump_string("format_str = ", specifier_start);
          fatal_error("printf: unsupported format specifier");
      }
    } else if (*format_str == '%') {
      mod = true;
      specifier_start = format_str;
      // Reset the state machine
      width_text = precision_text = has_width = has_precision = 0;
      state = PRINTF_STATE_FLAGS;
    }

    // Keep accumulating the format string
    format_str += 1;
  }

  // Dump the remaining format string
  append_glo_decl(printf_call(format_start, format_str, params_text, false));
}
#endif

text comp_fun_call_code(ast node, ast assign_to) {
  ast name = get_child__('(', IDENTIFIER, node, 0);
  ast params = get_child_('(', node, 1);
  int name_id = get_val_(IDENTIFIER, name);
  ast param;
  text res;

#ifdef SH_AVOID_PRINTF_USE
  if (assign_to == 0) {
    if (((name_id == PUTS_ID || name_id == PUTSTR_ID || name_id == PRINTF_ID)
        && (param = list_singleton(params)) != 0
        && get_op(param) == STRING)) { // puts("..."), putstr("..."), printf("...")
      return printf_call(STRING_BUF(get_val_(STRING, param)), 0, 0, true);
    } else if (name_id == PRINTF_ID && params != 0 && get_op(car(params)) == STRING) { // printf("...", ...)
      handle_printf_call(STRING_BUF(get_val_(STRING, car(params))), tail(params));
      return 0;
    }
#ifdef SH_INLINE_PUTCHAR
    else if (name_id == PUTCHAR_ID && (param = list_singleton(params)) != 0) { // putchar with 1 param
      return comp_putchar_inline(param);
    }
#endif
#ifdef SH_INLINE_EXIT
    else if (name_id == EXIT_ID && (param = list_singleton(params)) != 0) { // exit with 1 param
      res = comp_rvalue(param, RVALUE_CTX_BASE);
      return string_concat(wrap_str_lit("exit "), res);
    }
#endif
  }
#endif

       if (name_id == MALLOC_ID)  { runtime_use_malloc = true; }
  else if (name_id == FREE_ID)    { runtime_use_free = true; }
  else if (name_id == FOPEN_ID)   { runtime_use_fopen = true; }
  else if (name_id == FCLOSE_ID)  { runtime_use_fclose = true; }
  else if (name_id == FGETC_ID)   { runtime_use_fgetc = true; }
  else if (name_id == READ_ID)    { runtime_use_read = true; }
  else if (name_id == WRITE_ID)   { runtime_use_write = true; }
  else if (name_id == OPEN_ID)    { runtime_use_open = true; }
  else if (name_id == CLOSE_ID)   { runtime_use_close = true; }
#ifndef SH_INLINE_PUTCHAR
  else if (name_id == PUTCHAR_ID) { runtime_use_putchar = true; }
#endif
#ifndef SH_INLINE_EXIT
  else if (name_id == EXIT_ID)    { runtime_use_exit = true; }
#endif
#ifndef SH_MINIMAL_RUNTIME
  else if (name_id == GETCHAR_ID) { runtime_use_getchar = true; }
  else if (name_id == PRINTF_ID)  { runtime_use_printf = true; }
#endif

  if (assign_to) res = comp_lvalue(assign_to);
  else res = wrap_str_lit("__");

  return string_concat3(
    function_name(get_val_(IDENTIFIER, name)),
    wrap_char(' '),
    concatenate_strings_with(res, fun_call_params(params), wrap_char(' '))
  );
}

void comp_fun_call(ast node, ast assign_to) {
  text res = comp_fun_call_code(node, assign_to);
  if (res) append_glo_decl(res);
}

void comp_assignment(ast lhs, ast rhs) {
  int lhs_op = get_op(lhs);
  if (lhs_op == IDENTIFIER || lhs_op == '[' || lhs_op == '*'
#ifdef SUPPORT_STRUCT_UNION
    || lhs_op == ARROW
#endif
    ) {
    if (get_op(rhs) == '(') {
      comp_fun_call(rhs, lhs);
    } else {
      // If lhs is an identifier, we use x=$((...)) instead of : $((x = ...)).
      // This is unless the right hand side is an assignment, in which case we
      // generate everything in 1 arithmetic expansion for symmetry.
      //
      // Note: On certain shells there seems to be a conversion when entering and
      // exiting arithmetic expansions, meaning that the `x=$((...))` may not
      // always be equivalent to `: $((x =  ...))`.
      if (lhs_op == IDENTIFIER && get_op(rhs) != '=') {
        append_glo_decl(string_concat3(comp_lvalue(lhs), wrap_char('='), comp_rvalue(rhs, RVALUE_CTX_BASE)));
      } else {
        append_glo_decl(string_concat5(wrap_str_lit(": $(("), comp_lvalue(lhs), wrap_str_lit(" = "), comp_rvalue(rhs, RVALUE_CTX_ARITH_EXPANSION), wrap_str_lit("))")));
      }
    }
  } else {
    dump_node(lhs);
    fatal_error("comp_assignment: unknown lhs");
  }
}

bool comp_body(ast node, STMT_CTX stmt_ctx) {
  int start_in_tail_position = in_tail_position;
  int start_cgc_locals = cgc_locals;

  in_tail_position = false;

  while (node != 0) {
    // Last statement of body is in tail position if the body itself is in tail position
    if (get_op(get_child_('{', node, 1)) != '{') in_tail_position = start_in_tail_position;
    if (comp_statement(get_child_('{', node, 0), stmt_ctx)) break; // Statement always returns => block is terminated
    node = get_child_('{', node, 1);
  }

  cgc_locals = start_cgc_locals;
  return node != 0; // If node is not null, it means the block was terminated early
}

// Assemble switch pattern from case and default statements.
// Case and default statements are like labelled statements, meaning that they
// wrap the next statement. This function unwraps the next statements until a
// non-case statement is found.
// Because the non-case statement must be compiled as well, it is returned via
// the last_stmt global variable.
ast last_stmt;
text make_switch_pattern(ast statement) {
  text str = 0;

  while (1) { // statement will never be null
    switch (get_op(statement)) {
      case DEFAULT_KW:
        str = wrap_char('*');
        statement = get_child_(DEFAULT_KW, statement, 0);
        break;

      case CASE_KW:
        // This is much more permissive than what a C compiler would allow,
        // but Shell allows matching on arbitrary expression in case
        // patterns so it's fine. If we wanted to do this right, we'd check
        // that the pattern is a numeric literal or an enum identifier.
        str = concatenate_strings_with(str, comp_rvalue(get_child_(CASE_KW, statement, 0), RVALUE_CTX_BASE), wrap_char('|'));
        statement = get_child_(CASE_KW, statement, 1);
        break;

      default:
        if (str == 0) fatal_error("Expected case in switch. Fallthrough is not supported.");
        last_stmt = statement;
        return string_concat(str, wrap_char(')'));
    }
  }
}

bool comp_switch(ast node) {
  ast statement;
  int start_cgc_locals = cgc_locals;

  append_glo_decl(string_concat3(
      wrap_str_lit("case "),
      comp_rvalue(get_child_(SWITCH_KW, node, 0), RVALUE_CTX_BASE),
      wrap_str_lit(" in")
    ));

  cgc_add_enclosing_switch(in_tail_position);
  nest_level += 1;

  node = get_child_(SWITCH_KW, node, 1);

  if(get_op(node) == CASE_KW) {
    // This is for the edge case where the entire 'statement' part of < switch ( expression ) statement >
    // is a single < case constant-expression : statement >
    // therefore we wrap the case statement with a block statement to simplify down to the typical syntax
    node = new_ast2('{', node, 0);
  }

  if (node == 0 || get_op(node) != '{') fatal_error("comp_statement: switch without body");
  while (get_op(node) == '{') {
    statement = get_child_('{', node, 0);
    node = get_child_('{', node, 1);

    append_glo_decl(make_switch_pattern(statement));
    statement = last_stmt; // last_stmt is set by make_switch_pattern

    nest_level += 1;

    // Since we don't know if the switch is exhaustive, we can't compile in tail
    // position mode, see comment below.
    in_tail_position = false;

    // We keep compiling statements until we encounter a statement that returns or breaks.
    // Case and default nodes contain the first statement of the block so we process that one first.
    if (!comp_statement(statement, STMT_CTX_SWITCH)) {
      while (get_op(node) == '{') {
        statement = get_child_('{', node, 0);
        node = get_child_('{', node, 1);
        if (comp_statement(statement, STMT_CTX_SWITCH)) break;
      }
    }

    nest_level -= 1;
    append_glo_decl(wrap_str_lit(";;"));
  }

  nest_level -= 1;
  append_glo_decl(wrap_str_lit("esac"));

  cgc_locals = start_cgc_locals;

  // Returning not-false is only important for nested switch statements.
  // It could be useful to remove the need for the redundant trailing return
  // when nesting switch statements that we know are exhaustive such as in
  // eval_constant.
  //
  // I tried to make it return true if all cases of a switch end with a return
  // but it wasn't working well because we don't know if the switch delimits the
  // conditional block until it ends and so in_tail_position must be set to
  // false which defeats the point of removing the trailing return (since the
  // switch is not compiled in tail position mode even if it turns out to be the
  // case.
  return false;
}

bool comp_if(ast node, STMT_CTX stmt_ctx) {
  int start_glo_decl_idx;
  bool termination_lhs = false;
  bool termination_rhs = false;
  int start_cgc_locals = cgc_locals;

  bool else_if = stmt_ctx & STMT_CTX_ELSE_IF;
  stmt_ctx = stmt_ctx & ~STMT_CTX_ELSE_IF; // Clear STMT_CTX_ELSE_IF bit to not pass it to the next if statement

  append_glo_decl(string_concat3(
          wrap_str_lit(else_if ? "elif " : "if "),
          comp_rvalue(get_child_(IF_KW, node, 0), else_if ? RVALUE_CTX_TEST_ELSEIF : RVALUE_CTX_TEST),
          wrap_str_lit(" ; then")
        ));

  nest_level += 1;
  start_glo_decl_idx = glo_decl_ix;
  termination_lhs = comp_statement(get_child_(IF_KW, node, 1), stmt_ctx);
  // ifs cannot be empty so we insert ':' if it's empty
  if (!any_active_glo_decls(start_glo_decl_idx)) append_glo_decl(wrap_char(':'));
  nest_level -= 1;

  if (get_child_(IF_KW, node, 2) != 0) {
    // Compile sequence of if else if using elif
    if (get_op(get_child_(IF_KW, node, 2)) == IF_KW) {
      termination_rhs = comp_if(get_child_(IF_KW, node, 2), stmt_ctx | STMT_CTX_ELSE_IF); // STMT_CTX_ELSE_IF => next if stmt will use elif
    } else {
      append_glo_decl(wrap_str_lit("else"));
      nest_level += 1;
      start_glo_decl_idx = glo_decl_ix;
      termination_rhs = comp_statement(get_child_(IF_KW, node, 2), stmt_ctx & ~STMT_CTX_ELSE_IF); // Clear STMT_CTX_ELSE_IF bit
      if (!any_active_glo_decls(start_glo_decl_idx)) append_glo_decl(wrap_char(':'));
      nest_level -= 1;
    }
  }
  if (!else_if) append_glo_decl(wrap_str_lit("fi"));

  if (stmt_ctx & STMT_CTX_SWITCH && termination_lhs ^ termination_rhs) {
    fatal_error("Early break out of a switch case is unsupported");
  }

  cgc_locals = start_cgc_locals;

  return termination_lhs && termination_rhs;
}

// Function for compiling while, do_while and for loops
// last_line and loop_end_stmt are mutually exclusive
// last_line is the last line of the loop
// loop_end_stmt is the statement that should be executed at the end of the for loop (increment, etc.)
bool comp_loop(text cond, ast body, ast loop_end_stmt, text last_line, STMT_CTX stmt_ctx) {
  // Save loop end actions from possible outer loop
  int start_cgc_locals = cgc_locals;
  int start_glo_decl_idx;
  bool always_returns = false;
  int loop_binding;

  cgc_add_enclosing_loop();

  loop_binding = cgc_locals;

  // This is a little bit of a hack, but it makes things so much simpler.
  // Because we need to capture the code for the end of loop actions
  // (increment, etc.), and those can be any statement expression, we somehow
  // need to get the text generated by the comp_statement call. Instead of
  // modifying comp_statement to accomodate this, we just remove the code
  // generated by comp_statement using undo_glo_decls and save the indices of
  // the declarations of the loop end actions so they can replayed later.
  if (loop_end_stmt) {
    heap[loop_binding + 2] = glo_decl_ix;
    comp_statement(loop_end_stmt, stmt_ctx);
    undo_glo_decls(heap[loop_binding + 2]);
    heap[loop_binding + 3] = glo_decl_ix;
  }

  append_glo_decl(string_concat3(wrap_str_lit("while "), cond ? cond : wrap_char(':'), wrap_str_lit("; do")));
  nest_level += 1;
  start_glo_decl_idx = glo_decl_ix;
  always_returns = comp_statement(body, stmt_ctx);
  append_glo_decl(last_line);
  replay_glo_decls(heap[loop_binding + 2], heap[loop_binding + 3], true);
  // while loops cannot be empty so we insert ':' if it's empty
  if (!any_active_glo_decls(start_glo_decl_idx)) append_glo_decl(wrap_char(':'));
  nest_level -= 1;
  append_glo_decl(wrap_str_lit("done"));
  cgc_locals = start_cgc_locals;

  // If the condition is always true and the loop always returns
  return cond == wrap_char(':') && always_returns;
}

bool comp_break() {
  int binding = cgc_lookup_enclosing_loop_or_switch(cgc_locals);
  if (binding == 0) fatal_error("comp_statement: break not in loop or switch");
  if (binding_kind(binding) == BINDING_LOOP) {
    append_glo_decl(wrap_str_lit("break"));
  }
  return true;
}

bool comp_continue() {
  int binding = cgc_lookup_enclosing_loop(cgc_locals);
  if (binding == 0) fatal_error("comp_statement: continue not in loop");
  replay_glo_decls(heap[binding + 2], heap[binding + 3], true);
  // We could remove the continue when in tail position, but it's not worth doing
  append_glo_decl(wrap_str_lit("continue"));
  return false;
}

bool comp_return(ast return_value) {
  int binding = cgc_lookup_enclosing_loop_or_switch(cgc_locals);
  int loop_depth = cgc_loop_depth(binding);

  // First we assign the return value...
  if (return_value != 0) {
    if (get_op(return_value) == '(') { // Check if function call
      comp_fun_call(return_value, new_dollar_ident(1));
    } else {
      append_glo_decl(string_concat3(
        wrap_str_lit(": $(($1 = "),
        comp_rvalue(return_value, RVALUE_CTX_ARITH_EXPANSION),
        wrap_str_lit("))")
      ));
    }
  }

  // ...and then we take care of the control flow part of the return statement
  // SWITCH blocks specify if they are in tail position
  if (binding != 0 && binding_kind(binding) == BINDING_SWITCH) {
    in_tail_position |= heap[binding + 2];
  }

  if (in_tail_position && binding != 0) {
    // If in a loop or switch in tail position, we may be able to redirect the
    // control flow to the functions prologue and avoid having to endlet the
    // local variables. The cases where this is possible are:
    //  - When in a loop, using break (loop_depth > 0).
    //  - When in a switch surrounded by a loop, using break (loop_depth > 1).
    //  - For switch with no outer loop, by doing nothing (;; will break out of the switch) (loop_depth == 0).
    //
    // This means we only need a break statement when loop_depth != 0
    if (loop_depth >= 2) {
      append_glo_decl(string_concat(wrap_str_lit("break "), wrap_int(loop_depth)));
    } else if (loop_depth == 1) {
      append_glo_decl(wrap_str_lit("break"));
    }
  } else if (!in_tail_position) {
    rest_loc_var_fixups = cons(append_glo_decl_fixup(), rest_loc_var_fixups);
    append_glo_decl(wrap_str_lit("return"));
  }

  return true;
}

void comp_var_decls(ast node) {
  ast var_decl;

  switch (get_child_(DECLS, node, 1)) {
    // AUTO_KW and REGISTER_KW can simply be ignored.
    case EXTERN_KW:
    case STATIC_KW:
      fatal_error("Extern and static storage class specifier not supported on local variables");
      break;
  }
  node = get_child_opt_(DECLS, LIST, node, 0);
  while (node != 0) {
    // Add to local env and cummulative env, then initialize
    var_decl = car_(DECL, node);
    assert_var_decl_is_safe(var_decl, true);
    add_var_to_local_env(var_decl, BINDING_VAR_LOCAL);
    if (get_child_(DECL, var_decl, 2) != 0) { // Initializer
      comp_assignment(get_child__(DECL, IDENTIFIER, var_decl, 0), get_child_(DECL, var_decl, 2));
    }
    node = tail(node); // Next variable
  }
}

// Returns whether the statement always returns/breaks.
// This is used to delimit the end of conditional blocks of switch statements.
bool comp_statement(ast node, STMT_CTX stmt_ctx) {
  int op;
  text str;

  if (node == 0) return false; // Empty statement never returns

  op = get_op(node);

  gensym_ix = 0; // Reuse gensym names for each statement

  if (op == IF_KW) {
    return comp_if(node, stmt_ctx);
  } else if (op == WHILE_KW) {
    return comp_loop(comp_rvalue(get_child_(WHILE_KW, node, 0), RVALUE_CTX_TEST),
                     get_child_(WHILE_KW, node, 1),
                     0, // No loop end statement
                     0, // No last line
                     stmt_ctx
                     );
  } else if (op == DO_KW) {
    return comp_loop(wrap_str_lit(":"),
                     get_child_(DO_KW, node, 0),
                     0, // No loop end statement
                     string_concat(comp_rvalue(get_child_(DO_KW, node, 1), RVALUE_CTX_TEST), wrap_str_lit(" || break")),
                     stmt_ctx
                     );
  } else if (op == FOR_KW) {
    comp_statement(get_child_(FOR_KW, node, 0), STMT_CTX_DEFAULT); // Assuming this statement never returns...

    str = wrap_char(':'); // Empty statement
    if (get_child_(FOR_KW, node, 1)) {
      str = comp_rvalue(get_child_(FOR_KW, node, 1), RVALUE_CTX_TEST);
    }

    return comp_loop(str,
                     get_child_(FOR_KW, node, 3), // Body
                     get_child_(FOR_KW, node, 2), // End of loop statement
                     0, // No last line
                     stmt_ctx
                     );
  } else if (op == SWITCH_KW) {
    return comp_switch(node);
  } else if (op == BREAK_KW) {
    return comp_break(); // Break out of switch statement
  } else if (op == CONTINUE_KW) {
    return comp_continue(); // Continue to next iteration of loop
  } else if (op == RETURN_KW) {
    return comp_return(get_child_(RETURN_KW, node, 0));
  } else if (op == '(') { // six.call
    comp_fun_call(node, 0);
    return false;
  } else if (op == '{') { // six.compound
    return comp_body(node, stmt_ctx);
  } else if (op == '=') { // six.x=y
    comp_assignment(get_child_('=', node, 0), get_child_('=', node, 1));
    return false;
  } else if (op == ':') {
    // Labelled statement are not very useful as gotos are not supported in the
    // Shell backend, but we still emit a label comment for readability.
    append_glo_decl(string_concat3(wrap_str_lit("# "), wrap_str_pool(get_val_(IDENTIFIER, get_child_(':', node, 0))), wrap_char(':')));
    return comp_statement(get_child_(':', node, 1), stmt_ctx);
  } else if (op == GOTO_KW) {
    fatal_error("goto statements not supported");
    return false;
  } else if (get_op(node) == CASE_KW || get_op(node) == DEFAULT_KW) {
    fatal_error("case/default must be at the beginning of a switch conditional block");
    return false;
  } else if (op == DECLS) {
    comp_var_decls(node);
    return false;
  } else {
    str = comp_rvalue(node, RVALUE_CTX_BASE);
    if (contains_side_effects) {
      append_glo_decl(string_concat(wrap_str_lit(": "), str));
    }
    return false;
  }
}

void comp_glo_fun_decl(ast node) {
  ast fun_decl = get_child__(FUN_DECL, DECL, node, 0);
  ast body = get_child_opt_(FUN_DECL, '{', node, 1);
  ast name_probe = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, fun_decl, 0));
  ast fun_type = get_child__(DECL, '(', fun_decl, 1);
  ast params = get_child_opt_('(', LIST, fun_type, 1);
  text let_params_text = 0;
  text function_comment = 0;
  int params_ix = 2; // Start at 2 because $1 is assigned to the return location
  ast decl;
  int save_loc_vars_fixup;
  int start_glo_decl_idx;

  if (body == -1) return; // ignore forward declarations

  top_level_stmt = false;

  handle_function_params(params);

  // If the function is main
  if (name_probe == MAIN_ID) {
    main_defined = true;
    // If main has parameters, we'll prepare the argc/argv values in the epilogue.
    if (params != 0) runtime_use_make_argv = true;
  }

#ifdef SH_INITIALIZE_PARAMS_WITH_LET
  let_params_text = let_params(params);
#endif

  // Show the mapping between the function parameters and $1, $2, etc.
  while (params != 0) {
    decl = car_(DECL, params);
#ifdef SH_INITIALIZE_PARAMS_WITH_LET
    if (is_constant_type(get_child_(DECL, decl, 1))) { // Skip constant params
#endif
      function_comment = concatenate_strings_with(function_comment, string_concat3(wrap_str_pool(get_val_(IDENTIFIER, get_child_(DECL, decl, 0))), wrap_str_lit(": $"), wrap_int(params_ix)), wrap_str_lit(", "));
#ifdef SH_INITIALIZE_PARAMS_WITH_LET
    }
#endif
    params = tail(params);
    params_ix += 1;
  }
  if (function_comment != 0) function_comment = string_concat(wrap_str_lit(" # "), function_comment);


  append_glo_decl(string_concat4(
    function_name(name_probe),
    wrap_str_lit("() {"),
    let_params_text,
    function_comment
  ));

  in_tail_position = true;
  nest_level += 1;
  start_glo_decl_idx = glo_decl_ix;

  save_loc_vars_fixup = append_glo_decl_fixup(); // Fixup is done after compiling body

#ifndef SH_INITIALIZE_PARAMS_WITH_LET
  // Initialize parameters
  params = get_child_opt_('(', LIST, fun_type, 1); // Reload params because params is now = 0
  params_ix = 2;
  while (params != 0) {
    decl = car_(DECL, params);
    if (!is_constant_type(get_child_(DECL, decl, 1))) { // Skip constant params
      // We need to use the $1, $2, etc. identifiers because the function
      // parameters are not in the local environment.
      comp_assignment(get_child_(DECL, decl, 0), new_dollar_ident(params_ix));
    }
    params = tail(params);
    params_ix += 1;
  }
#endif

  comp_body(body, STMT_CTX_DEFAULT);

  // Set local environment to cummulative for the save_local_vars/restore_local_vars
  cgc_locals = cgc_locals_fun;

  append_glo_decl(restore_local_vars(params_ix - 1));

  // We only know the full set of temporary variables after compiling the function body.
  // So we fixup the calls to save_vars and unsave_vars at the end.
  fixup_glo_decl(save_loc_vars_fixup, save_local_vars());
  while (rest_loc_var_fixups != 0) {
    fixup_glo_decl(car(rest_loc_var_fixups), restore_local_vars(params_ix - 1));
    rest_loc_var_fixups = tail(rest_loc_var_fixups);
  }

  // functions cannot be empty so we insert ':' if it's empty
  if (!any_active_glo_decls(start_glo_decl_idx)) append_glo_decl(wrap_char(':'));

  nest_level -= 1;

  append_glo_decl(wrap_str_lit("}\n"));
}

void comp_glo_var_decl(ast node) {
  ast name = get_child__(DECL, IDENTIFIER, node, 0);
  ast type = get_child_(DECL, node, 1);
  ast init = get_child_(DECL, node, 2);
  int arr_len, init_len;
  text args = 0;

  if (get_op(type) == '(') return; // Ignore function declarations

  // TODO: Add enum/struct/union to env if it's not already there
  // handle_enum_struct_union_type_decl(type);

  assert_var_decl_is_safe(node, false);

  if (get_op(type) == '[') { // Array declaration
    arr_len = get_child_('[', type, 1);

    // If the array is initialized with a string, we want to call defstr on the
    // string, and then initialize the array variable with the variable passed
    // to defstr.
    if (init != 0 && get_op(init) == STRING) {
      init_len = heap[get_val_(STRING, init) + 4] + 1; // string_end - string_start
      if (arr_len != 0 && arr_len < init_len) {
        fatal_error("Array type is too small for initializer");
      }
      comp_defstr(name, get_val_(STRING, init), arr_len != 0 ? arr_len : init_len);
    } else {
      // If the array is initialized with an initializer list, we want to pass
      // the list of values to the defarr function. Because the array size is
      // optional, we need to calculate the size of the array from the
      // initializer list if it's not provided.
#ifdef SUPPORT_COMPLEX_INITIALIZER
      if (init != 0) {
        if (get_op(init) != INITIALIZER_LIST) fatal_error("Array declaration with invalid initializer");
        init = get_child_(INITIALIZER_LIST, init, 0);

        runtime_use_initialize = true;
        init_len = initializer_list_len(init);
        args = comp_initializer_list(init, arr_len);
        if (arr_len == 0) {
          arr_len = init_len;
        } else if (arr_len < init_len) {
          fatal_error("Array type is too small for initializer");
        }
      }
#endif // SUPPORT_COMPLEX_INITIALIZER

      if (arr_len == 0) {
        fatal_error("Array declaration without size or initializer list");
      }

      runtime_defarr();

      append_glo_decl(
        concatenate_strings_with(
          string_concat4(
            wrap_str_lit("defarr "),
            global_var(get_val_(IDENTIFIER, name)),
            wrap_char(' '),
            wrap_int(arr_len)),
          args,
          wrap_char(' '))
        );
    }
  } else {
#ifdef SUPPORT_ADDRESS_OF_OP
    runtime_defglo();
    append_glo_decl(
      string_concat4(
        wrap_str_lit("defglo "),
        global_var(get_val_(IDENTIFIER, name)),
        wrap_char(' '),
        comp_rvalue(init, VALUE_CTX_BASE)
      )
    );
#else
    if (init == 0) init = new_ast0(INTEGER, 0);
    comp_assignment(name, init);
#endif
  }
}

void comp_assignment_constant(text constant_name, ast rhs) {
  append_glo_decl(string_concat4(wrap_str_lit("readonly "), constant_name, wrap_char('='), comp_rvalue(rhs, RVALUE_CTX_BASE)));
}

// Enums are just like global variables, but they are readonly.
// Since anything that's not a local variable is considered global, this makes
// it easy to implement enums.
void comp_enum_cases(ast ident, ast cases) {
  ast cas;
  if (ident != 0) {
    append_glo_decl(string_concat3(wrap_str_lit("# "), wrap_str_pool(get_val_(IDENTIFIER, ident)), wrap_str_lit(" enum declaration")));
  } else {
    append_glo_decl(wrap_str_lit("# Enum declaration"));
  }

  while (cases != 0) {
    cas = car_('=', cases);
    comp_assignment_constant(global_var(get_val_(IDENTIFIER, get_child__('=', IDENTIFIER, cas, 0))), get_child_('=', cas, 1));
    cases = tail(cases);
  }
}

#ifdef SUPPORT_STRUCT_UNION

// Struct member access is implemented like array indexing. Each member is mapped
// to a readonly variable containing the offset of the member and accessing to
// s->a is equivalent to *(s + a).
//
// For example, for the struct:
//
//   struct Point {
//     int x;
//     int y;
//   }
//
//   Point *p = malloc(sizeof(Point));
//   p->y = 42;
//
// The following code is generated:
//
//   readonly __x=0
//   readonly __y=1
//   readonly __sizeof__Point=2
//
//   _malloc p $((__sizeof__Point))
//   : $(( _$((p + __x)) = 42 ))
//
// This approach doesn't work when the same member name is used in different
// structs, but it makes for readable code and is simple to implement.
// Because the member offset variables are declared as readonly, name conflicts
// will result in a runtime error when the shell program initializes.
void comp_struct(ast ident, ast members) {
  ast decl;
  int offset = new_ast0(INTEGER, 0);
  int field_type;
  if (ident != 0) {
    append_glo_decl(string_concat3(wrap_str_lit("# "), wrap_str_pool(get_val_(IDENTIFIER, ident)), wrap_str_lit(" struct member declarations")));
  } else {
    append_glo_decl(wrap_str_lit("# Struct member declarations"));
  }
  while (members != 0) {
    decl = car_(DECL, members);
    members = tail(members);
    field_type = get_child_(DECL, decl, 1);
    // Arrays and struct value types are not supported for now.
    // When we have type information on the local and global variables, we'll
    // be able to generate the correct code for these cases.
    if (get_op(field_type) == '[' || get_op(field_type) == STRUCT_KW) {
      fatal_error("Nested structures not supported by shell backend. Use a reference type instead.");
    }

    comp_assignment_constant(struct_member_var(get_child_opt_(DECL, IDENTIFIER, decl, 0)), offset);
    set_val(offset, get_val_(INTEGER, offset) - 1);
  }

  if (ident != 0) {
    comp_assignment_constant(struct_sizeof_var(ident), offset);
  }

  append_glo_decl(0); // newline
}

#endif

void handle_enum_struct_union_type_decl(ast type) {
  if (get_op(type) == ENUM_KW) {
    comp_enum_cases(get_child_opt_(ENUM_KW, IDENTIFIER, type, 1), get_child_(ENUM_KW, type, 2));
  }
#ifdef SUPPORT_STRUCT_UNION
  else if (get_op(type) == STRUCT_KW) {
    comp_struct(get_child_opt_(STRUCT_KW, IDENTIFIER, type, 1), get_child_(STRUCT_KW, type, 2));
  } else if (get_op(type) == UNION_KW) {
    fatal_error("handle_enum_struct_union_type_decl: union not supported");
  }
#endif

  // If not an enum, struct, or union, do nothing
}

// For now, we don't do anything with the declarations in a typedef.
// The only thing we need to do is to call handle_enum_struct_union_type_decl
// on the type specifier.
void handle_typedef(ast node) {
  ast decls = get_child__(TYPEDEF_KW, LIST, node, 0);
  ast decl = car_(DECL, decls);
  ast type = get_child_(DECL, decl, 1);

  handle_enum_struct_union_type_decl(get_type_specifier(type));
}

// This function compiles 1 top level declaration at the time.
// The supported top level declarations are:
//   - global variable declarations
//   - global variable assignments
//   - function declarations
//   - enum declarations
//   - struct declarations
void comp_glo_decl(ast node) {
  ast declarations;
  int op = get_op(node);
  fun_gensym_ix = 0;

  top_level_stmt = true;

  if (op == '=') { // Assignments
   comp_assignment(get_child_('=', node, 0), get_child_('=', node, 1));
  } else if (op == DECLS) { // Variable declarations
    // AUTO_KW and REGISTER_KW can simply be ignored. STATIC_KW is the default
    // storage class for global variables since pnut-sh only supports 1
    // translation unit.
    if (get_child_(DECLS, node, 1) == EXTERN_KW) fatal_error("Extern storage class specifier not supported");
    declarations = get_child__(DECLS, LIST, node, 0);
    while (declarations != 0) { // Multiple variable declarations
      comp_glo_var_decl(car_(DECL, declarations));
      declarations = tail(declarations);
    }
  } else if (op == FUN_DECL) {
    comp_glo_fun_decl(node);
  } else if (op == TYPEDEF_KW) {
    handle_typedef(node);
  } else if (op == ENUM_KW
#ifdef SUPPORT_STRUCT_UNION
    || op == STRUCT_KW || op == UNION_KW
#endif
  ) {
    handle_enum_struct_union_type_decl(node);
  } else {
    dump_node(node);
    fatal_error("comp_glo_decl: unexpected declaration");
  }
}

void prologue() {
  putstr("#!/bin/sh\n");
#ifdef RT_UNSAFE_HEAP
  putstr("set -e -f\n");
#else
  putstr("set -e -u -f\n");
#endif
  putstr("LC_ALL=C\n\n");
}

void epilogue() {
  int c;

  if (any_character_used) {
    putstr("# Character constants\n");
    for(c = 0; c < 256; c += 1) {
      if (characters_useds[c / CHARACTERS_BITFIELD_SIZE] & 1 << (c % CHARACTERS_BITFIELD_SIZE)) {
        putstr("readonly ");
        print_text(character_ident(c));
        putchar('='); putint(c); putchar('\n');
      }
    }
  }

  putstr("# Runtime library\n");
  produce_runtime();

  if (main_defined) {
    putstr("__code=0; # Exit code\n");
    if (runtime_use_make_argv) {
      putstr("make_argv $(($# + 1)) \"$0\" \"$@\" # Setup argc/argv\n");
      putstr("_main __code $(($# + 1)) $__argv");
    } else {
      putstr("_main __code");
    }
    putstr("\nexit $__code\n");
  }
}

// Initialize local and synthetic variables used by function
text initialize_function_variables() {
  int env = cgc_locals_fun;
  ast ident;
  text res = 0;
  int counter = fun_gensym_ix;

  // Internal variables
  while (counter > 0) {
    ident = new_fresh_ident(counter);
    res = concatenate_strings_with(res, format_special_var(ident, false), wrap_str_lit(" = "));
    counter -= 1;
  }

  // Local variables and parameters
  while (env != 0) {
    ident = binding_ident(env);
    if (binding_kind(env) != BINDING_PARAM_LOCAL || !is_constant_type(heap[env + 4])) { // Skip constant params
      res = concatenate_strings_with(res, local_var(ident), wrap_str_lit(" = "));
    }
    env = binding_next(env);
  }

  if (res != 0) {
    res = string_concat3(wrap_str_lit(": $(("), res, wrap_str_lit(" = 0))"));
  }
  return res;
}

void codegen_begin() {
  init_comp_context();
  prologue();
}

int max_text_alloc = 0;
int cumul_text_alloc = 0;
void codegen_glo_decl(ast decl) {
  int var_init_fixup;
#ifndef ONE_PASS_GENERATOR_NO_EARLY_OUTPUT
  // Reset text and glo decls buffers
  glo_decl_ix = 0;
  text_alloc = 1;
#endif

  // Reset local environment
  cgc_locals = cgc_locals_fun = 0;
  cgc_fs = 1; // 1 to account for the return location parameter

  var_init_fixup = append_glo_decl_fixup();
  comp_glo_decl(decl);
  fixup_glo_decl(var_init_fixup, initialize_function_variables());
#ifndef ONE_PASS_GENERATOR_NO_EARLY_OUTPUT
  print_glo_decls();
#endif

  // Statistics
  max_text_alloc = max_text_alloc > text_alloc ? max_text_alloc : text_alloc;
  cumul_text_alloc += text_alloc;
}

void codegen_end() {
#ifdef ONE_PASS_GENERATOR_NO_EARLY_OUTPUT
  print_glo_decls();
#endif
  epilogue();
#ifdef PRINT_MEMORY_STATS
  printf("\n# string_pool_alloc=%d heap_alloc=%d max_text_alloc=%d cumul_text_alloc=%d\n", string_pool_alloc, heap_alloc, max_text_alloc, cumul_text_alloc);
#endif
}
