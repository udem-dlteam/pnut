// POSIX shell codegen

#include "sh-runtime.c"

void handle_shell_include() {
  FILE* shell_include_fp;
  int c;
  if (tok == STRING) {
    // Include pack_string and unpack_string functions
    // since they will likely be used in the included file
    runtime_use_put_pstr = true;
    runtime_use_unpack_string = true;
    // Include the file as-is without any preprocessing
    shell_include_fp = fopen_source_file(STRING_BUF(val), include_stack->dirname);
    while ((c = fgetc(shell_include_fp)) != EOF) {
      putchar(c);
    }
    putchar('\n');
    fclose(shell_include_fp);
    get_tok_macro(); // Skip the string
  } else {
    putstr("tok="); putint(tok); putchar('\n');
    syntax_error("expected string to #include_shell directive");
  }
}

// codegen

#define text int
#define TEXT_POOL_SIZE 1000000
#ifdef PNUT_CC
// On pnut, intptr_t is not defined
#define intptr_t int
#endif
intptr_t text_pool[TEXT_POOL_SIZE];
int text_alloc = 1; // Start at 1 because 0 is the empty text

// Text pool nodes
enum TEXT_NODES {
  TEXT_TREE,
  TEXT_INTEGER,
  TEXT_STRING,
  TEXT_ESCAPED
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

text wrap_int(int i) {
  if (text_alloc + 2 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_INTEGER);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(i);
  return (text_alloc += 2) - 2;
}

text escape_text(text t, bool for_printf) {
  if (text_alloc + 3 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");

  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_ESCAPED);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(t);
  text_pool[text_alloc + 2] = TEXT_FROM_INT(for_printf);
  return (text_alloc += 3) - 3;
}

text string_concat(text t1, text t2) {
  if (text_alloc + 4 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_TREE);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(2);
  text_pool[text_alloc + 2] = TEXT_FROM_INT(t1);
  text_pool[text_alloc + 3] = TEXT_FROM_INT(t2);
  return (text_alloc += 4) - 4;
}

text string_concat3(text t1, text t2, text t3) {
  if (text_alloc + 5 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_TREE);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(3);
  text_pool[text_alloc + 2] = TEXT_FROM_INT(t1);
  text_pool[text_alloc + 3] = TEXT_FROM_INT(t2);
  text_pool[text_alloc + 4] = TEXT_FROM_INT(t3);
  return (text_alloc += 5) - 5;
}

text string_concat4(text t1, text t2, text t3, text t4) {
  if (text_alloc + 6 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_TREE);
  text_pool[text_alloc + 1] = TEXT_FROM_INT(4);
  text_pool[text_alloc + 2] = TEXT_FROM_INT(t1);
  text_pool[text_alloc + 3] = TEXT_FROM_INT(t2);
  text_pool[text_alloc + 4] = TEXT_FROM_INT(t3);
  text_pool[text_alloc + 5] = TEXT_FROM_INT(t4);
  return (text_alloc += 6) - 6;
}

text string_concat5(text t1, text t2, text t3, text t4, text t5) {
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
// text wrap_str(char *s) {
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
text wrap_str_imm(char *s, char *end) {
  if (text_alloc + 3 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_INT(TEXT_STRING);
  text_pool[text_alloc + 1] = TEXT_FROM_PTR(s);
  text_pool[text_alloc + 2] = TEXT_FROM_PTR(end); // end of string address. 0 for null-terminated strings
  return (text_alloc += 3) - 3;
}

text wrap_str_lit(char *s) {
  return wrap_str_imm(s, 0);
}

text wrap_str_pool(int s) {
  return wrap_str_imm(string_pool + s, 0);
}

text concatenate_strings_with(text t1, text t2, text sep) {
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
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_STRING)) {
    print_escaped_string((char*) text_pool[t + 1],  (char*) text_pool[t + 2], for_printf);
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_ESCAPED)) {
    fatal_error("Cannot escape a string that is already escaped");
  } else {
    printf("\nt=%d %d\n", t, TEXT_TO_INT(text_pool[t]));
    fatal_error("print_escaped_text: unexpected string tree node");
  }
}

void print_text(text t) {
  int i;

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
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_STRING)) {
    if (TEXT_TO_INT(text_pool[t + 2]) == 0) {
      putstr((char*) text_pool[t + 1]);
    } else {
      i = text_pool[t + 1]; // start
      while (i < TEXT_TO_INT(text_pool[t + 2])) {
        putchar(string_pool[i]);
        i += 1;
      }
    }
  } else if (text_pool[t] == TEXT_FROM_INT(TEXT_ESCAPED)) {
    print_escaped_text(TEXT_TO_INT(text_pool[t + 1]), TEXT_TO_INT(text_pool[t + 2]));
  } else {
    printf("\nt=%d %d\n", t, TEXT_TO_INT(text_pool[t]));
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
#define CHARACTERS_BITFIELD_SIZE 16
int characters_useds[16];       // Characters used in string literals. Bitfield, each int stores 16 bits, so 16 ints in total
bool any_character_used = false; // If any character is used
ast rest_loc_var_fixups = 0;    // rest_loc_vars call to fixup after compiling a function
bool main_defined = false;      // If the main function is defined
bool main_returns = false;      // If the main function returns a value
bool top_level_stmt = true;     // If the current statement is at the top level

// Internal identifier node types. These
enum IDENTIFIER_TYPE {
  IDENTIFIER_INTERNAL = 600,
  IDENTIFIER_STRING,
  IDENTIFIER_DOLLAR,
  IDENTIFIER_EMPTY
};

void init_comp_context() {
  int i = 0;
  // Initialize characters_useds table
  while (i < 16) {
    characters_useds[i] = 0;
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
  glo_decls[glo_decl_ix] = -nest_level; // Negative value to indicate that it's a fixup
  glo_decls[glo_decl_ix + 1] = 1; // If it's active or not. Used by undo_glo_decls and replay_glo_decls
  glo_decls[glo_decl_ix + 2] = 0;
  glo_decl_ix += 3;
  return glo_decl_ix - 3;
}

void fixup_glo_decl(int fixup_ix, text decl) {
  if (glo_decls[fixup_ix] >= 0) fatal_error("fixup_glo_decl: invalid fixup");

  glo_decls[fixup_ix] = -glo_decls[fixup_ix]; // Make nest level positive
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

// Similar to env_var, but doesn't use the local environment and assumes that the
// identifier is internal or global. This is faster than env_var when we know that
// the variable is not local.
text format_special_var(ast ident, ast prefixed_with_dollar) {
  int op = get_op(ident);
  if (op == IDENTIFIER_INTERNAL) {
    return string_concat(wrap_str_lit("__t"), get_val_(IDENTIFIER_INTERNAL, ident));
  } else if (op == IDENTIFIER_STRING) {
    return string_concat(wrap_str_lit("__str_"), get_val_(IDENTIFIER_STRING, ident));
  } else if (op == IDENTIFIER_DOLLAR) {
    if (prefixed_with_dollar) {
      if (get_val_(IDENTIFIER_DOLLAR, ident) <= 9) {
        return wrap_int(get_val_(IDENTIFIER_DOLLAR, ident));
      } else {
        return string_concat3(wrap_char('{'), wrap_int(get_val_(IDENTIFIER_DOLLAR, ident)), wrap_char('}'));
      }
    } else {
      if (get_val_(IDENTIFIER_DOLLAR, ident) <= 9) {
        return string_concat(wrap_char('$'), wrap_int(get_val_(IDENTIFIER_DOLLAR, ident)));
      } else {
        return string_concat3(wrap_str_lit("${"), wrap_int(get_val_(IDENTIFIER_DOLLAR, ident)), wrap_char('}'));
      }
    }
  } else if (op == IDENTIFIER_EMPTY) {
    return wrap_str_lit("__");
  } else {
    printf("op=%d %c", op, op);
    fatal_error("format_special_var: unknown identifier type");
    return 0;
  }
}

text struct_member_var(ast member_name_ident) {
  return string_concat(wrap_str_lit("__"), wrap_str_pool(probe_string(get_val_(IDENTIFIER, member_name_ident))));
}

text struct_sizeof_var(ast struct_name_ident) {
  return string_concat(wrap_str_lit("__sizeof__"), wrap_str_pool(probe_string(get_val_(IDENTIFIER, struct_name_ident))));
}

text global_var(ast ident) {
  return string_concat(wrap_char('_'), wrap_str_pool(probe_string(ident)));
}

text env_var_with_prefix(ast ident, ast prefixed_with_dollar) {
  if (get_op(ident) == IDENTIFIER) {
    if (cgc_lookup_var(get_val_(IDENTIFIER, ident), cgc_locals)) {
      // TODO: Constant param optimization
      if (get_val_(IDENTIFIER, ident) == ARGV_ID) {
        return wrap_str_lit("argv_");
      } else {
        return wrap_str_pool(probe_string(get_val_(IDENTIFIER, ident)));
      }
    } else {
      return global_var(get_val_(IDENTIFIER, ident));
    }
  } else {
    return format_special_var(ident, prefixed_with_dollar);
  }
}

text env_var(ast ident) {
  return env_var_with_prefix(ident, false);
}

text function_name(int ident_tok) {
  return string_concat(wrap_char('_'), wrap_str_pool(probe_string(ident_tok)));
}

ast fresh_ident() {
  gensym_ix += 1;
  if (gensym_ix > fun_gensym_ix) {
    fun_gensym_ix = gensym_ix;
  }
  if (gensym_ix > max_gensym_ix) {
    max_gensym_ix = gensym_ix;
  }
  return new_ast0(IDENTIFIER_INTERNAL, wrap_int(gensym_ix));
}

ast fresh_string_ident(int string_probe) {
  // Strings are interned, meaning that the same string used twice will have the
  // same address. We use the token tag to mark the string as already defined.
  // This allows comp_defstr to use the same string variable for the same string.
  if (heap[string_probe + 3] == 0) { // tag defaults to 0
    string_counter += 1;
    heap[string_probe + 3] = string_counter - 1;
  }
  return new_ast0(IDENTIFIER_STRING, wrap_int(heap[string_probe + 3]));
}

void add_var_to_local_env(ast decl, enum BINDING kind) {
  int ident_probe = get_child_(VAR_DECL, decl, 0);

  // Make sure we're not shadowing an existing local variable
  if (cgc_lookup_var(ident_probe, cgc_locals)) {
    putstr("var="); putstr(string_pool + probe_string(ident_probe)); putchar('\n');
    fatal_error("Variable is already in local environment");
  }

  // The var is not part of the environment, so we add it.
  cgc_add_local_var(kind, ident_probe, get_child_(VAR_DECL, decl, 1));
}

void add_fun_params_to_local_env(ast lst) {
  while (lst != 0) {
    add_var_to_local_env(get_child__(',', VAR_DECL, lst, 0), BINDING_PARAM_LOCAL);
    lst = get_child_opt_(',', ',', lst, 1);
  }
}

// Since global and internal variables are prefixed with _, we restrict the name
// of variables to not start with _. Also, because some shells treat some
// variables as special, we prevent their use.
//
// Also, the shell backend doesn't support variables with aggregate types.
void assert_var_decl_is_safe(ast variable, bool local) { // Helper function for assert_idents_are_safe
  ast ident_probe = get_child_(VAR_DECL, variable, 0);
  char* name = string_pool + probe_string(ident_probe);
  ast type = get_child_(VAR_DECL, variable, 1);
  if (name[0] == '_'
  || (name[0] != '\0' && name[1] == '_' && name[2] == '\0')) { // Check for a_ variables that could conflict with character constants
    printf("%s ", name);
    fatal_error("variable name is invalid. It can't start or end with '_'.");
  }

  // IFS is a special shell variable that's overwritten by certain.
  // In zsh, writing to argv assigns to $@, so we map argv to argv_, and forbid argv_.
  // This check only applies to local variables because globals are prefixed with _.
  if (local && (ident_probe == ARGV__ID || ident_probe == IFS_ID)) {
    printf("%s ", name);
    fatal_error("variable name is invalid. It can't be 'IFS' or 'argv_'.");
  }

  // Local variables don't correspond to memory locations, and can't store
  // more than 1 number/pointer.
  if (local && (get_op(type) == '[' || (get_op(type) == STRUCT_KW && get_stars(type) == 0))) {
    printf("%s ", name);
    fatal_error("array/struct value type is not supported for shell backend. Use a reference type instead.");
  }
}

void check_param_decls(ast lst) {
  while (lst != 0) {
    assert_var_decl_is_safe(get_child__(',', VAR_DECL, lst, 0), true);
    lst = get_child_(',', lst, 1);
  }
}

#ifdef SH_SAVE_VARS_WITH_SET
// Save the value of local variables to positional parameters
text save_local_vars() {
  int env = cgc_locals_fun;
  ast ident;
  text res = 0;
  int counter = fun_gensym_ix;

  while (env != 0) {
    // TODO: Constant param optimization
    // if (variable_is_constant_param(local_var)) continue;
    ident = new_ast0(IDENTIFIER, binding_ident(env));
    res = concatenate_strings_with(string_concat(wrap_char('$'), env_var_with_prefix(ident, true)), res, wrap_char(' '));
    env = binding_next(env);
  }

  while (counter > 0) {
    ident = new_ast0(IDENTIFIER_INTERNAL, wrap_int(fun_gensym_ix - counter + 1));
    res = concatenate_strings_with(res, string_concat(wrap_char('$'), env_var_with_prefix(ident, true)), wrap_char(' '));
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
    env_non_cst_size += 1;
    env = binding_next(env);

    // TODO: Constant param optimization
    // if(variable_is_constant_param(local_var)) continue;
  }

  env = cgc_locals_fun;

  while (env != 0) {
    // TODO: Constant param optimization
    // if (variable_is_constant_param(local_var)) continue;
    ident = new_ast0(IDENTIFIER, binding_ident(env));
    res = concatenate_strings_with(string_concat5(wrap_str_lit("$(("), env_var_with_prefix(ident, true), wrap_str_lit(" = $"), format_special_var(new_ast0(IDENTIFIER_DOLLAR, params_count + env_non_cst_size - local_var_pos), true), wrap_str_lit("))")), res, wrap_char(' '));
    local_var_pos += 1;
    env = binding_next(env);
  }

  while (counter > 0) {
    ident = new_ast0(IDENTIFIER_INTERNAL, wrap_int(fun_gensym_ix - counter + 1));
    res = concatenate_strings_with(res, string_concat5(wrap_str_lit("$(("), env_var_with_prefix(ident, true), wrap_str_lit(" = $"), format_special_var(new_ast0(IDENTIFIER_DOLLAR, params_count + local_var_pos + 1), true), wrap_str_lit("))")), wrap_char(' '));
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
  ast ident;
  text res = 0;
  int params_ix = 2;

  while (params != 0) {
    // TODO: Constant param optimization
    ident = new_ast0(IDENTIFIER, get_child_(VAR_DECL, get_child__(',', VAR_DECL, params, 0), 0));
    res = concatenate_strings_with(res, string_concat4(wrap_str_lit("let "), env_var_with_prefix(ident, false), wrap_char(' '), format_special_var(new_ast0(IDENTIFIER_DOLLAR, params_ix), false)), wrap_str_lit("; "));
    params = get_child_opt_(',', ',', params, 1);
    params_ix += 1;
  }

  runtime_use_local_vars |= res != 0;

  return res;
}
#endif

text save_local_vars() {
  int env = cgc_locals_fun;
  ast ident;
  text res = 0;
  int counter = fun_gensym_ix;

  while (counter > 0) {
    ident = new_ast0(IDENTIFIER_INTERNAL, wrap_int(counter));
    res = concatenate_strings_with(string_concat(wrap_str_lit("let "), format_special_var(ident, true)), res, wrap_str_lit("; "));
    counter -= 1;
  }

  while (env != 0) {
#ifdef SH_INITIALIZE_PARAMS_WITH_LET
    if (binding_kind(env) != BINDING_PARAM_LOCAL) { // Skip params
#else
    // TODO: Constant param optimization
    // Constant function parameters are assigned to $1, $2, ... and don't need to be saved
    if (true)
    // if (!variable_is_constant_param(local_var)) {
#endif
      ident = new_ast0(IDENTIFIER, binding_ident(env));
      res = concatenate_strings_with(string_concat(wrap_str_lit("let "), env_var_with_prefix(ident, true)), res, wrap_str_lit("; "));
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

  while (counter > 0) {
    ident = new_ast0(IDENTIFIER_INTERNAL, wrap_int(counter));
    res = concatenate_strings_with(res, format_special_var(ident, false), wrap_char(' '));
    counter -= 1;
  }

  while (env != 0) {
    // Constant function parameters are assigned to $1, $2, ... and don't need to be saved
    // TODO: Constant param optimization
    // if (!variable_is_constant_param(local_var)) {
      ident = new_ast0(IDENTIFIER, binding_ident(env));
      res = concatenate_strings_with(res, env_var(ident), wrap_char(' '));
    // }

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
    printf("op=%d %c\n", op, op);
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
    printf("op=%d %c\n", op, op);
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
  } else {
    if      (c == '\0') return wrap_str_lit("__NUL__");
    else if (c == '\n') return wrap_str_lit("__NEWLINE__");
    else if (c == ' ')  return wrap_str_lit("__SPACE__");
    else if (c == '!')  return wrap_str_lit("__EXCL__");
    else if (c == '"')  return wrap_str_lit("__DQUOTE__");
    else if (c == '#')  return wrap_str_lit("__SHARP__");
    else if (c == '$')  return wrap_str_lit("__DOLLAR__");
    else if (c == '%')  return wrap_str_lit("__PERCENT__");
    else if (c == '&')  return wrap_str_lit("__AMP__");
    else if (c == '\'') return wrap_str_lit("__QUOTE__");
    else if (c == '(')  return wrap_str_lit("__LPAREN__");
    else if (c == ')')  return wrap_str_lit("__RPAREN__");
    else if (c == '*')  return wrap_str_lit("__STAR__");
    else if (c == '+')  return wrap_str_lit("__PLUS__");
    else if (c == ',')  return wrap_str_lit("__COMMA__");
    else if (c == '-')  return wrap_str_lit("__MINUS__");
    else if (c == '.')  return wrap_str_lit("__PERIOD__");
    else if (c == '/')  return wrap_str_lit("__SLASH__");
    else if (c == ':')  return wrap_str_lit("__COLON__");
    else if (c == ';')  return wrap_str_lit("__SEMICOLON__");
    else if (c == '<')  return wrap_str_lit("__LT__");
    else if (c == '=')  return wrap_str_lit("__EQ__");
    else if (c == '>')  return wrap_str_lit("__GT__");
    else if (c == '?')  return wrap_str_lit("__QUESTION__");
    else if (c == '@')  return wrap_str_lit("__AT__");
    else if (c == '^')  return wrap_str_lit("__CARET__");
    else if (c == '[')  return wrap_str_lit("__LBRACK__");
    else if (c == '\\') return wrap_str_lit("__BACKSLASH__");
    else if (c == ']')  return wrap_str_lit("__RBRACK__");
    else if (c == '_')  return wrap_str_lit("__UNDERSCORE__");
    else if (c == '`')  return wrap_str_lit("__BACKTICK__");
    else if (c == '{')  return wrap_str_lit("__LBRACE__");
    else if (c == '|')  return wrap_str_lit("__BAR__");
    else if (c == '}')  return wrap_str_lit("__RBRACE__");
    else if (c == '~')  return wrap_str_lit("__TILDE__");
    else if (c == '\a') return wrap_str_lit("__ALARM__");
    else if (c == '\b') return wrap_str_lit("__BACKSPACE__");
    else if (c == '\f') return wrap_str_lit("__PAGE__");
    else if (c == '\r') return wrap_str_lit("__RET__");
    else if (c == '\t') return wrap_str_lit("__TAB__");
    else if (c == '\v') return wrap_str_lit("__VTAB__");
    else { fatal_error("Unknown character"); return 0; }
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
  ast sub1, sub2;

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
  sub2 = get_child_('(', node, 1);
  if (sub2 != 0) { // Check if not an empty list
    sub1 = node;   // For 1 param, the parent node is the fun call node
    // If there are 2 or more params, we traverse the ',' nodes ...
    while (get_op(sub2) == ',') {
      sub1 = sub2;; // .. and the parent node is the ',' node
      set_child(sub1, 0, handle_side_effects_go(get_child_(',', sub2, 0), executes_conditionally));
      sub2 = get_child_(',', sub2, 1);
    }
    // Handle the last argument
    set_child(sub1, 1, handle_side_effects_go(sub2, executes_conditionally));
  }

  // All the temporary variables used for the function parameters can be
  // reused after the function call, so resetting the gensym counter.
  gensym_ix = start_gensym_ix;

  sub1 = new_ast2('=', assign_to, node);
  sub1 = new_ast2(',', sub1, 0);
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
    if (op == IDENTIFIER || op == IDENTIFIER_INTERNAL || op == IDENTIFIER_STRING || op == IDENTIFIER_DOLLAR || op == INTEGER || op == CHARACTER) {
      return node;
    } else if (op == STRING) {
      /* We must initialize strings before the expression */
      sub1 = fresh_string_ident(get_val_(STRING, node));
      literals_inits = new_ast2(',', new_ast2('=', sub1, get_val_(STRING, node)), literals_inits);
      return sub1;
    } else {
      printf("handle_side_effects_go: op=%d %c", op, op);
      fatal_error("unexpected operator");
      return 0;
    }
  } else if (nb_children == 1) {
    if (op == '&' || op == '*' || op == '+' || op == '-' || op == '~' || op == '!' || op == PARENS) {
      // TODO: Reuse ast node?
      return new_ast1(op, handle_side_effects_go(child0, executes_conditionally));
    } else if (op == PLUS_PLUS_PRE || op == MINUS_MINUS_PRE || op == PLUS_PLUS_POST || op == MINUS_MINUS_POST) {
      contains_side_effects = true;
      return new_ast1(op, handle_side_effects_go(child0, executes_conditionally));
    } else if (op == SIZEOF_KW) {
      return node; // sizeof is a compile-time operator
    } else {
      printf("1: op=%d %c", op, op);
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
      || op == '.' || op == ARROW ) {
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
      printf("2: op=%d %c", op, op);
      fatal_error("unexpected operator");
      return 0;
    }
  } else if (nb_children == 3) {
    if (op == '?') {
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
      printf("3: op=%d %c\n", op, op);
      fatal_error("unexpected operator");
      return 0;
    }
  } else if (nb_children == 4) {
    printf("4: op=%d %c\n", op, op);
    fatal_error("unexpected operator");
    return 0;
  } else {
    printf("5: op=%d %c with %d children\n", op, op, get_nb_children(node));
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

void comp_defstr(ast ident, int string_probe) {
  char *string_start = string_pool + heap[string_probe + 1];
  char *string_end = string_start + heap[string_probe + 4];

  if (top_level_stmt) {
    // If defstr is used at the top level, it needs to be included beforehand
    runtime_defstr();
  } else {
    runtime_use_defstr = true;
  }

  append_glo_decl(string_concat5( wrap_str_lit("defstr ")
                                , format_special_var(ident, false)
                                , wrap_str_lit(" \"")
                                , escape_text(wrap_str_imm(string_start, string_end), false)
                                , wrap_char('\"')));
}

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
    side_effect = get_child__(',', '=', test_side_effects, 0);
    test_side_effects_code =
      string_concat3(test_side_effects_code,
                     comp_fun_call_code(get_child_('=', side_effect, 1), get_child_('=', side_effect, 0)),
                     wrap_str_lit("; "));
    test_side_effects = get_child_(',', test_side_effects, 1);
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

int non_parenthesized_operand(ast node) {
  while (get_op(node) == PARENS) {
    node = get_child_(PARENS, node, 0);
  }

  return node;
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
    } else if (op == CHARACTER) {
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
    } else if (op == STRING) {
      fatal_error("comp_rvalue_go: string should have been removed by handle_side_effects");
      return 0;
    } else {
      printf("op=%d %c", op, op);
      fatal_error("comp_rvalue_go: unknown rvalue with nb_children == 0");
      return 0;
    }
  } else if (nb_children == 1) {
    if (op == '*') {
      // Setting context to RVALUE_CTX_BASE even if it's wrapped in $(( ... )) because we
      // need another layer of wrapping if it's a complex expression, i.e. not a
      // literal or a variable.
      sub1 = comp_rvalue_go(child0, RVALUE_CTX_BASE, 0, op);
      return wrap_if_needed(false, context, test_side_effects, string_concat(wrap_char('_'), sub1), outer_op, op);
    } else if (op == '+' || op == PARENS) {
      // +x is equivalent to x
      return comp_rvalue_go(child0, context, test_side_effects, outer_op);
    } else if (op == '-') {
      // Check if the rest of ast is a literal, if so directly return the negated value.
      // Note: I think this can be simplified by not wrapped in () in the else case.
      if (get_op(child0) == INTEGER) {
        return wrap_in_condition_if_needed(context, test_side_effects, wrap_int(get_val_(INTEGER, child0)));
      } else {
        sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, op);
        return wrap_if_needed(false, context, test_side_effects, string_concat3(wrap_str_lit("-("), sub1, wrap_char(')')), outer_op, op);
      }
    } else if (op == '~') {
      sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      return wrap_if_needed(false, context, test_side_effects, string_concat3(wrap_str_lit("~("), sub1, wrap_char(')')), outer_op, op);
    } else if (op == '!') {
      sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      return wrap_if_needed(true, context, test_side_effects, string_concat(wrap_char('!'), sub1), outer_op, op);
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
      if (get_op(child0) == INT_KW
       || get_op(child0) == CHAR_KW
       || get_op(child0) == VOID_KW
       || get_op(child0) == ENUM_KW
       || (( get_op(child0) == STRUCT_KW || get_op(child0) == UNION_KW)
          && get_child(child0, 0) >= 1)) { // If it's a pointer
        return wrap_in_condition_if_needed(context, test_side_effects, wrap_int(1));
      } else if (get_op(child0) == STRUCT_KW) {
        return wrap_if_needed(false, context, test_side_effects, struct_sizeof_var(get_child__(STRUCT_KW, IDENTIFIER, child0, 1)), outer_op, op);
      } else {
        fatal_error("comp_rvalue_go: sizeof is not supported for this type or expression");
        return 0;
      }
    } else if (op == '&') {
      return wrap_if_needed(false, context, test_side_effects, comp_lvalue_address(child0), outer_op, op);
    } else {
      printf("1: op=%d %c", op, op);
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
    } else if (op == ARROW) { // member access is implemented like array access
      sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      sub2 = struct_member_var(child1);
      return wrap_if_needed(false, context, test_side_effects, string_concat5(wrap_str_lit("_$(("), sub1, wrap_str_lit(" + "), sub2, wrap_str_lit("))")), outer_op, op);
    } else if (op == EQ_EQ || op == EXCL_EQ || op == LT_EQ || op == GT_EQ || op == '<' || op == '>') {
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
    } else if (op == AMP_AMP || op == BAR_BAR) {
      fatal_error("comp_rvalue_go: && and || should have 4 children by that point");
      return 0;
    } else {
      fatal_error("comp_rvalue_go: unknown rvalue with 2 children");
      return 0;
    }
  } else if (nb_children == 3) {
    if (op == '?') {
      sub1 = comp_rvalue_go(child0, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      sub2 = comp_rvalue_go(child1, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      sub3 = comp_rvalue_go(child2, RVALUE_CTX_ARITH_EXPANSION, 0, op);
      return wrap_if_needed(true, context, test_side_effects, string_concat5(sub1, op_to_str(op), sub2, wrap_str_lit(": "), sub3), outer_op, op);
      return 0;
    } else {
      printf("op=%d %c\n", op, op);
      fatal_error("comp_rvalue_go: unknown rvalue with 3 children");
      return 0;
    }
  } else if (nb_children == 4) {
    if (op == AMP_AMP || op == BAR_BAR) {
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
        sub1 = non_parenthesized_operand(child0); // un-parenthesized lhs
        sub2 = non_parenthesized_operand(child1); // un-parenthesized rhs

        // if lhs is && or ||, and different from the current operator
        if ((get_op(sub1) == AMP_AMP || get_op(sub1) == BAR_BAR) && get_op(sub1) != op) {
          sub1 = comp_rvalue_go(sub1, RVALUE_CTX_TEST, child2, op);
          sub1 = string_concat3(wrap_str_lit("{ "), sub1, wrap_str_lit("; }"));
        } else {
          sub1 = comp_rvalue_go(sub1, RVALUE_CTX_TEST, child2, op);
        }

        // if rhs is && or ||, and different from the current operator
        if ((get_op(sub2) == AMP_AMP || get_op(sub2) == BAR_BAR) && get_op(sub2) != op) {
          sub2 = comp_rvalue_go(sub2, RVALUE_CTX_TEST, child3, op);
          sub2 = string_concat3(wrap_str_lit("{ "), sub2, wrap_str_lit("; }"));
        } else {
          sub2 = comp_rvalue_go(sub2, RVALUE_CTX_TEST, child3, op);
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
      printf("op=%d %c\n", op, op);
      fatal_error("comp_rvalue_go: unknown rvalue with 4 children");
      return 0;
    }
  } else {
    printf("op=%d %c\n", op, op);
    fatal_error("comp_rvalue_go: unknown rvalue with >4 children");
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
    side_effect = get_child__(',', '=', literals_inits, 0);
    comp_defstr(get_child_('=', side_effect, 0), get_child_('=', side_effect, 1));
    literals_inits = get_child_opt_(',', ',', literals_inits, 1);
  }

  // We don't want to call defstr on every iteration, so we only capture fun
  // calls, not literal initialization. That's unless it's for a elif statement,
  // because the previous block is not executed so the def_str calls must be
  // placed inline with the rest of the condition.
  if (context != RVALUE_CTX_TEST_ELSEIF)
    fun_call_decl_start = glo_decl_ix;

  while (replaced_fun_calls2 != 0) {
    side_effect = get_child__(',', '=', replaced_fun_calls2, 0);
    comp_fun_call(get_child_('=', side_effect, 1), get_child_('=', side_effect, 0));
    replaced_fun_calls2 = get_child_opt_(',', ',', replaced_fun_calls2, 1);
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
  } else if (op == ARROW) {
    sub1 = comp_rvalue(get_child_(ARROW, node, 0), RVALUE_CTX_ARITH_EXPANSION);
    sub2 = struct_member_var(get_child_(ARROW, node, 1));
    return string_concat3(sub1, wrap_str_lit(" + "), sub2);
  } else if (op == CAST) {
    return comp_lvalue_address(get_child_(CAST, node, 1));
  } else {
    printf("op=%d %c\n", op, op);
    fatal_error("comp_lvalue_address: unknown lvalue");
    return 0;
  }
}

text comp_lvalue(ast node) {
  int op = get_op(node);
  text sub1;
  text sub2;

  if (op == IDENTIFIER || op == IDENTIFIER_INTERNAL || op == IDENTIFIER_STRING || op == IDENTIFIER_EMPTY || op == IDENTIFIER_DOLLAR) {
    return env_var(node);
  } else if (op == '[') {
    sub1 = comp_rvalue(get_child_('[', node, 0), RVALUE_CTX_ARITH_EXPANSION);
    sub2 = comp_rvalue(get_child_('[', node, 1), RVALUE_CTX_ARITH_EXPANSION);
    return string_concat5(wrap_str_lit("_$(("), sub1, wrap_str_lit(" + "), sub2, wrap_str_lit("))"));
  } else if (op == '*') {
    sub1 = comp_rvalue(get_child_('*', node, 0), RVALUE_CTX_BASE);
    return string_concat(wrap_char('_'), sub1);
  } else if (op == ARROW) {
    sub1 = comp_rvalue(get_child_(ARROW, node, 0), RVALUE_CTX_ARITH_EXPANSION);
    sub2 = struct_member_var(get_child_(ARROW, node, 1));
    return string_concat5(wrap_str_lit("_$(("), sub1, wrap_str_lit(" + "), sub2, wrap_str_lit("))"));
  } else if (op == CAST) {
    return comp_lvalue(get_child_(CAST, node, 1));
  } else {
    printf("op=%d %c\n", op, op);
    fatal_error("comp_lvalue: unknown lvalue");
    return 0;
  }
}

text fun_call_params(ast params) {
  ast param;
  text code_params = 0;

  if (params != 0) { // Check if not an empty list
    while (get_op(params) == ',') {
      param = comp_rvalue(get_child_(',', params, 0), RVALUE_CTX_BASE);
      code_params = concatenate_strings_with(code_params, param, wrap_char(' '));
      params = get_child_(',', params, 1);
    }
    param = comp_rvalue(params, RVALUE_CTX_BASE); // Last parameter
    code_params = concatenate_strings_with(code_params, param, wrap_char(' '));
  }

  return code_params;
}

// Workaround because #if defined(SH_AVOID_PRINTF_USE) || defined(SH_INLINE_PUTCHAR) doesn't work
#ifdef SH_AVOID_PRINTF_USE
#define INCLUDE_COMP_PUTCHAR_INLINE
#endif

#ifdef SH_INLINE_PUTCHAR
#define INCLUDE_COMP_PUTCHAR_INLINE
#endif

#ifdef INCLUDE_COMP_PUTCHAR_INLINE
text comp_putchar_inline(ast param) {
  text res;
  ast ident;

  if (get_op(param) == CHARACTER && get_val_(CHARACTER, param) >= 32 && get_val_(CHARACTER, param) <= 126) { // Printable ASCII characters
    return string_concat3(wrap_str_lit("printf \""), escape_text(wrap_char(get_val_(CHARACTER, param)), true), wrap_char('\"'));
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
      if (get_op(params) == ',') {
        param = get_child_(',', params, 0);
        params = get_child_(',', params, 1);
      } else {
        param = params;
        params = 0;
      }
    }

    if (mod) {
      switch (*format_str) {
        case ' ': case '#': case '+': case '-': case '0': // Flags
          // Flags correspond to 0x20,0x23,0x2b,0x2d,0x30 which are spread over
          // 16 bits meaning we can easily convert char -> bit if we wanted to.
          if (state != PRINTF_STATE_FLAGS) fatal_error("Invalid printf format: Flags must come before width and precision");
          break;

        // Width or precision literal
        case '1': case '2': case '3':
        case '4': case '5': case '6':
        case '7': case '8': case '9':
          if (state != PRINTF_STATE_FLAGS && state != PRINTF_STATE_PRECISION) fatal_error("Invalid printf format: Width or precision already specified by a number");
          while ('0' <= *format_str && *format_str <= '9') format_str += 1; // Skip the rest of the number
          has_width = state == PRINTF_STATE_FLAGS ? true : has_width;
          has_precision = state == PRINTF_STATE_PRECISION ? true : has_precision;
          state += 1;      // Move to the next state (PRINTF_STATE_FLAGS => PRINTF_STATE_WIDTH, PRINTF_STATE_PRECISION => PRINTF_STATE_SPECIFIER)
          format_str -= 1; // Reprocess non-numeric character
          break;

        // Precision
        case '.':
          if (state >= PRINTF_STATE_PRECISION) fatal_error("Invalid printf format: precision already specified");
          state = PRINTF_STATE_PRECISION;
          break;

        case '*':
          if (param == 0) fatal_error("Not enough parameters for printf");
          if (state == PRINTF_STATE_FLAGS) {
            width_text = comp_rvalue(param, RVALUE_CTX_BASE);
            has_width = true;
          } else if (state == PRINTF_STATE_PRECISION) {
            precision_text = comp_rvalue(param, RVALUE_CTX_BASE);
            has_precision = true;
          } else {
            fatal_error("Width or precision already specified by a number");
          }
          param = 0;
          break;

        case '%':
          if (state != PRINTF_STATE_FLAGS) fatal_error("Cannot use flags, width or precision with %%");
          mod = false;
          break;

        // The following options are the same between the shell's printf and C's printf
        case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
          if (param == 0) fatal_error("Not enough parameters for printf");
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
          if (param == 0) fatal_error("Not enough parameters for printf");
          // TODO: Find way to support width that's not too verbose
          if (has_width)   fatal_error("Width not supported for %c");
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
          if (param == 0)       fatal_error("Not enough parameters for printf");
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
          printf("specifier=%s\n", specifier_start);
          printf("format char='%c'\n", *format_str);
          fatal_error("Unsupported format specifier");
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
  text res;

#ifdef SH_AVOID_PRINTF_USE
  if (get_op(assign_to) == IDENTIFIER_EMPTY) {
    if (((name_id == PUTS_ID || name_id == PUTSTR_ID || name_id == PRINTF_ID)
        && params != 0 && get_op(params) == STRING)) { // puts("..."), putstr("..."), printf("...")
      return printf_call(STRING_BUF(get_val_(STRING, params)), 0, 0, true);
    } else if (name_id == PRINTF_ID && get_op(get_child(params, 0)) == STRING) {
      handle_printf_call(STRING_BUF(get_val_(STRING, get_child(params, 0))), get_child(params, 1));
      return 0;
    }
#ifdef SH_INLINE_PUTCHAR
    else if (name_id == PUTCHAR_ID && params != 0 && get_op(params) != ',') { // putchar with 1 param
      return comp_putchar_inline(params);
    }
#endif
#ifdef SH_INLINE_EXIT
    else if (name_id == EXIT_ID && params != 0 && get_op(params) != ',') { // exit with 1 param
      res = comp_rvalue(params, RVALUE_CTX_BASE);
      return string_concat(wrap_str_lit("exit "), res);
    }
#endif
  }
#endif

       if (name_id == PUTCHAR_ID) { runtime_use_putchar = true; }
  else if (name_id == GETCHAR_ID) { runtime_use_getchar = true; }
  else if (name_id == EXIT_ID)    { runtime_use_exit = true; }
  else if (name_id == MALLOC_ID)  { runtime_use_malloc = true; }
  else if (name_id == FREE_ID)    { runtime_use_free = true; }
  else if (name_id == PRINTF_ID)  { runtime_use_printf = true; }
  else if (name_id == FOPEN_ID)   { runtime_use_fopen = true; }
  else if (name_id == FCLOSE_ID)  { runtime_use_fclose = true; }
  else if (name_id == FGETC_ID)   { runtime_use_fgetc = true; }
  else if (name_id == READ_ID)    { runtime_use_read = true; }
  else if (name_id == WRITE_ID)   { runtime_use_write = true; }
  else if (name_id == OPEN_ID)    { runtime_use_open = true; }
  else if (name_id == CLOSE_ID)   { runtime_use_close = true; }

  return string_concat3(
    function_name(get_val_(IDENTIFIER, name)),
    wrap_char(' '),
    concatenate_strings_with(comp_lvalue(assign_to), fun_call_params(params), wrap_char(' '))
  );
}

void comp_fun_call(ast node, ast assign_to) {
  text res = comp_fun_call_code(node, assign_to);
  if (res) append_glo_decl(res);
}

void comp_assignment(ast lhs, ast rhs) {
  int lhs_op = get_op(lhs);
  if (lhs_op == IDENTIFIER || lhs_op == '[' || lhs_op == '*' || lhs_op == ARROW) {
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
    printf("lhs_op=%d %c\n", lhs_op, lhs_op);
    fatal_error("unknown lhs");
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
      comp_fun_call(return_value, new_ast0(IDENTIFIER_DOLLAR, 1));
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
    rest_loc_var_fixups = new_ast2(',', append_glo_decl_fixup(), rest_loc_var_fixups);
    append_glo_decl(wrap_str_lit("return"));
  }

  return true;
}

void comp_var_decls(ast node) {
  ast var_decl;

  node = get_child_opt_(VAR_DECLS, ',', node, 0);
  while (node != 0) {
    // Add to local env and cummulative env, then initialize
    var_decl = get_child__(',', VAR_DECL, node, 0);
    add_var_to_local_env(var_decl, BINDING_VAR_LOCAL);
    if (get_child_(VAR_DECL, var_decl, 2) != 0) { // Initializer
      comp_assignment(new_ast0(IDENTIFIER, get_child_(VAR_DECL, var_decl, 0)), get_child_(VAR_DECL, var_decl, 2));
    }
#ifdef INITIALIZE_LOCAL_VARS_WITH_ZERO
    else {
      comp_assignment(new_ast0(IDENTIFIER, get_child_(VAR_DECL, var, 0)), new_ast0(INTEGER, 0));
    }
#endif
    node = get_child_opt_(',', ',', node, 1); // Next variable
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
    comp_fun_call(node, new_ast0(IDENTIFIER_EMPTY, 0)); // Reuse IDENTIFIER_EMPTY ast?
    return false;
  } else if (op == '{') { // six.compound
    return comp_body(node, stmt_ctx);
  } else if (op == '=') { // six.x=y
    comp_assignment(get_child_('=', node, 0), get_child_('=', node, 1));
    return false;
  } else if (op == ':') {
    // Labelled statement are not very useful as gotos are not supported in the
    // Shell backend, but we still emit a label comment for readability.
    append_glo_decl(string_concat3(wrap_str_lit("# "), wrap_str_pool(probe_string(get_val_(IDENTIFIER, get_child_(':', node, 0)))), wrap_char(':')));
    return comp_statement(get_child_(':', node, 1), stmt_ctx);
  } else if (op == GOTO_KW) {
    fatal_error("goto statements not supported");
    return false;
  } else if (get_op(node) == CASE_KW || get_op(node) == DEFAULT_KW) {
    fatal_error("case/default must be at the beginning of a switch conditional block");
    return false;
  } else if (op == VAR_DECLS) {
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
  ast name = get_child_(FUN_DECL, node, 0);
  ast fun_type = get_child_(FUN_DECL, node, 1);
  ast params = get_child_opt_(FUN_DECL, ',', node, 2);
  ast body = get_child_opt_(FUN_DECL, '{', node, 3);
  text trailing_txt = 0;
  int params_ix = 2; // Start at 2 because $1 is assigned to the return location
  ast var;
  int save_loc_vars_fixup;
  int start_glo_decl_idx;

  if (body == -1) return; // ignore forward declarations

  top_level_stmt = false;

  check_param_decls(params);
  add_fun_params_to_local_env(params);

  // If the function is main
  if (name == MAIN_ID) {
    main_defined = true;
    // If main has parameters. If so, we'll prepare the argc/argv values in the prologue.
    if (params != 0) runtime_use_make_argv = true;
    // Check if main returns an exit code.
    if (get_op(fun_type) != VOID_KW) main_returns = true;
  }

#ifdef SH_INITIALIZE_PARAMS_WITH_LET
  trailing_txt = let_params(params);
  if (trailing_txt != 0) trailing_txt = string_concat(wrap_char(' '), trailing_txt);
#endif

  if (trailing_txt == 0) {
    // Show the mapping between the function parameters and $1, $2, etc.
    while (params != 0) {
      var = get_child__(',', VAR_DECL, params, 0);
      trailing_txt = concatenate_strings_with(trailing_txt, string_concat3(wrap_str_pool(probe_string(get_child_(VAR_DECL, var, 0))), wrap_str_lit(": $"), wrap_int(params_ix)), wrap_str_lit(", "));
      params = get_child_(',', params, 1);
      params_ix += 1;
    }
    if (trailing_txt != 0) trailing_txt = string_concat(wrap_str_lit(" # "), trailing_txt);
  }

  append_glo_decl(string_concat3(
    function_name(name),
    wrap_str_lit("() {"),
    trailing_txt
  ));

  in_tail_position = true;
  nest_level += 1;
  start_glo_decl_idx = glo_decl_ix;

  save_loc_vars_fixup = append_glo_decl_fixup(); // Fixup is done after compiling body

#ifndef SH_INITIALIZE_PARAMS_WITH_LET
  // Initialize parameters
  params = get_child_opt_(FUN_DECL, ',', node, 2); // Reload params because params is now = 0
  params_ix = 2;
  while (params != 0) {
    var = get_child__(',', VAR_DECL, params, 0);

    // TODO: Constant param optimization
    // Constant parameters don't need to be initialized
    comp_assignment(new_ast0(IDENTIFIER, get_child_(VAR_DECL, var, 0)), new_ast0(IDENTIFIER_DOLLAR, params_ix));
    params = get_child_opt_(',', ',', params, 1);
    params_ix += 1;
  }
#endif

  comp_body(body, STMT_CTX_DEFAULT);
  // functions cannot be empty so we insert ':' if it's empty
  if (!any_active_glo_decls(start_glo_decl_idx)) append_glo_decl(wrap_char(':'));

  // Set local environment to cummulative for the save_local_vars/restore_local_vars
  cgc_locals = cgc_locals_fun;

  append_glo_decl(restore_local_vars(params_ix - 1));

  // We only know the full set of temporary variables after compiling the function body.
  // So we fixup the calls to save_vars and unsave_vars at the end.
  fixup_glo_decl(save_loc_vars_fixup, save_local_vars());
  while (rest_loc_var_fixups != 0) {
    fixup_glo_decl(get_child_(',', rest_loc_var_fixups, 0), restore_local_vars(params_ix - 1));
    rest_loc_var_fixups = get_child_opt_(',', ',', rest_loc_var_fixups, 1);
  }

  nest_level -= 1;

  append_glo_decl(wrap_str_lit("}\n"));
}

void comp_glo_var_decl(ast node) {
  ast name = get_child(node, 0);
  ast type = get_child(node, 1);
  ast init = get_child(node, 2);

  if (init == 0) init = new_ast0(INTEGER, 0);

  // TODO: Add enum/struct/union to env if it's not already there
  // handle_enum_struct_union_type_decl(type);

  assert_var_decl_is_safe(node, false);

  // Arrays of structs and struct value types are not supported for now.
  // When we have type information on the local and global variables, we'll
  // be able to generate the correct code for these cases.
  if ((get_op(type) == '['
    && get_op(get_child_('[', type, 1)) == STRUCT_KW
    && get_stars(get_child_('[', type, 1)) == 0)
    || (get_op(type) == STRUCT_KW && get_stars(type) == 0)) {
    printf("%s ", string_pool + probe_string(name));
    fatal_error("array of struct and struct value type are not supported in shell backend. Use a reference type instead.");
  }

  if (get_op(type) == '[') { // Array declaration
    runtime_defarr();
    append_glo_decl(
      string_concat4(
        wrap_str_lit("defarr "),
        env_var(new_ast0(IDENTIFIER, name)),
        wrap_char(' '),
        wrap_int(get_val_(INTEGER, get_child__('[', INTEGER, type, 0)))
      )
    );
  } else {
#ifdef SUPPORT_ADDRESS_OF_OP
    runtime_defglo();
    append_glo_decl(
      string_concat4(
        wrap_str_lit("defglo "),
        env_var(new_ast0(IDENTIFIER, name)),
        wrap_char(' '),
        comp_rvalue(init, VALUE_CTX_BASE)
      )
    );
#else
    comp_assignment(new_ast0(IDENTIFIER, name), init);
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
  if (ident != 0) {
    append_glo_decl(string_concat3(wrap_str_lit("# "), wrap_str_pool(probe_string(get_val_(IDENTIFIER, ident))), wrap_str_lit(" enum declaration")));
  } else {
    append_glo_decl(wrap_str_lit("# Enum declaration"));
  }
  while (get_op(cases) == ',') {
    comp_assignment_constant(env_var(get_child__(',', IDENTIFIER, cases, 0)), get_child_(',', cases, 1));
    cases = get_child_opt_(',', ',', cases, 2);
  }
}

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
  int offset = new_ast0(INTEGER, 0);
  int field_type;
  if (ident != 0) {
    append_glo_decl(string_concat3(wrap_str_lit("# "), wrap_str_pool(probe_string(get_val_(IDENTIFIER, ident))), wrap_str_lit(" struct member declarations")));
  } else {
    append_glo_decl(wrap_str_lit("# Struct member declarations"));
  }
  while (get_op(members) == ',') {
    field_type = get_child_(',', members, 1);
    comp_assignment_constant(struct_member_var(get_child_opt_(',', IDENTIFIER, members, 0)), offset);
    members = get_child_opt_(',', ',', members, 2);

    // Arrays and struct value types are not supported for now.
    // When we have type information on the local and global variables, we'll
    // be able to generate the correct code for these cases.
    if (get_op(field_type) == '[' || (get_op(field_type) == STRUCT_KW && get_stars(field_type) == 0)) {
      fatal_error("Nested structures not supported by shell backend. Use a reference type instead.");
    } else {
      set_val(offset, get_val_(INTEGER, offset) - 1);
    }
  }

  if (ident != 0) {
    comp_assignment_constant(struct_sizeof_var(ident), offset);
  }

  append_glo_decl(0); // newline
}

void handle_enum_struct_union_type_decl(ast type) {
  if (get_op(type) == ENUM_KW) {
    comp_enum_cases(get_child_opt_(ENUM_KW, IDENTIFIER, type, 1), get_child_(ENUM_KW, type, 2));
  } else if (get_op(type) == STRUCT_KW) {
    comp_struct(get_child_opt_(STRUCT_KW, IDENTIFIER, type, 1), get_child_(STRUCT_KW, type, 2));
  } else if (get_op(type) == UNION_KW) {
    fatal_error("handle_enum_struct_union_type_decl: union not supported");
  }

  // If not an enum, struct, or union, do nothing
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
  } else if (op == VAR_DECLS) { // Variable declarations
    declarations = get_child__(VAR_DECLS, ',', node, 0);
    while (declarations != 0) { // Multiple variable declarations
      comp_glo_var_decl(get_child__(',', VAR_DECL, declarations, 0));
      declarations = get_child_opt_(',', ',', declarations, 1);
    }
  } else if (op == FUN_DECL) {
    comp_glo_fun_decl(node);
  } else if (op == TYPEDEF_KW) {
    handle_enum_struct_union_type_decl(get_child_(TYPEDEF_KW, node, 1));
  } else if (op == ENUM_KW || op == STRUCT_KW || op == UNION_KW) {
    handle_enum_struct_union_type_decl(node);
  } else {
    printf("op=%d %c with %d children\n", op, op, get_nb_children(node));
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

  text main_args = 0;

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
    if (runtime_use_make_argv) {
      putstr("# Setup argc, argv\n");
      putstr("__argc_for_main=$(($# + 1))\n");
      putstr("make_argv $__argc_for_main \"$0\" \"$@\"; __argv_for_main=$__argv\n");
      main_args = wrap_str_lit(" $__argc_for_main $__argv_for_main");
    }

    if (main_returns) {
      putstr("__code=0; # Success exit code\n");
      print_text(string_concat3(wrap_str_lit("_main __code"), main_args, wrap_str_lit("; exit $__code\n")));
    } else {
      print_text(string_concat3(wrap_str_lit("_main __"), main_args, wrap_char('\n')));
    }
  }
}

// Initialize local and synthetic variables used by function
void initialize_function_variables() {
  int env = cgc_locals_fun;
  ast ident;
  text res = 0;
  int counter = fun_gensym_ix;

  while (counter > 0) {
    ident = new_ast0(IDENTIFIER_INTERNAL, wrap_int(counter));
    res = concatenate_strings_with(res, format_special_var(ident, false), wrap_str_lit(" = "));
    counter -= 1;
  }

  while (env != 0) {
    ident = new_ast0(IDENTIFIER, binding_ident(env));

    // TODO: Constant param optimization
    // if (!variable_is_constant_param(local_var)) {
      res = concatenate_strings_with(res, env_var(ident), wrap_str_lit(" = "));
    // }
    env = binding_next(env);
  }

  if (res != 0) {
    res = string_concat3(wrap_str_lit(": $(("), res, wrap_str_lit(" = 0))"));
    print_text(res);
    putchar('\n');
  }
}

void codegen_begin() {
  init_comp_context();
  prologue();
}

int max_text_alloc = 0;
int cumul_text_alloc = 0;
void codegen_glo_decl(ast decl) {
  comp_glo_decl(decl);
  initialize_function_variables();
  print_glo_decls();
  // Reset state
  glo_decl_ix = 0;
  cgc_locals = cgc_locals_fun = 0; // Reset local environment
  max_text_alloc = max_text_alloc > text_alloc ? max_text_alloc : text_alloc;
  cumul_text_alloc += text_alloc;
  text_alloc = 1;
}

void codegen_end() {
  epilogue();
#ifdef PRINT_MEMORY_STATS
  printf("\n# string_pool_alloc=%d heap_alloc=%d max_text_alloc=%d cumul_text_alloc=%d\n", string_pool_alloc, heap_alloc, max_text_alloc, cumul_text_alloc);
#endif
}
