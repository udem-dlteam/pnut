// POSIX shell codegen

#include "sh-runtime.c"

void handle_shell_include() {
  FILE* shell_include_fp;
  int c;
  get_tok();
  if (tok == STRING) {
    // Include the shell code from the file
    shell_include_fp = fopen(string_pool + val, "r");
    // Include pack_string and unpack_string functions
    // since they will likely be used in the included file
    runtime_use_pack_string = true;
    runtime_use_unpack_string = true;
    while ((c = fgetc(shell_include_fp)) != EOF) {
      putchar(c);
    }
    putchar('\n');
    fclose(shell_include_fp);
  } else {
    putstr("tok="); putint(tok); putchar('\n');
    syntax_error("expected string to #include_shell directive");
  }
}

void print_string_char(int c) {
  if (c == 7)       putstr("\\a");
  else if (c == 8)  putstr("\\b");
  else if (c == 12) putstr("\\f");
  else if (c == 10) putstr("\\n");
  else if (c == 13) putstr("\\r");
  else if (c == 9)  putstr("\\t");
  else if (c == 11) putstr("\\v");
  else if ((c == '\\') OR (c == '\'') OR (c == '\"')) { putstr("\\"); putchar(c); }
  else if ((c < 32) OR (c > 126)) { putstr("\\"); putint(c>>6); putint((c>>3)&7); putint(c&7); }
  else putchar(c);
}

/* codegen */

#define text int
#define TEXT_POOL_SIZE 1000000
int text_pool[TEXT_POOL_SIZE];
int text_alloc = 1; /* Start at 1 because 0 is the empty text */

// Text pool nodes
enum TEXT_NODES {
  TEXT_TREE,
  TEXT_INTEGER,
  TEXT_FROM_POOL,
  TEXT_ESCAPED
};

/* Place prototype of mutually recursive functions here */

text comp_lvalue_address(ast node);
text comp_lvalue(ast node);
text comp_fun_call_code(ast node, ast assign_to);
void comp_fun_call(ast node, ast assign_to);
void comp_body(ast node);
void comp_statement(ast node, int else_if);
void mark_mutable_variables_body(ast node);
void handle_enum_struct_union_type_decl(ast node);


/*
  Because concatenating strings is very expensive and a common operation, we
  use a tree structure to represent the concatenated strings. That way, the
  concatenation can be done in O(1).
  At the end of the codegen process, the tree will be flattened into a single
  string.
*/

#define wrap_char(c) (-c)

text wrap_int(int i) {
  if (text_alloc + 3 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_INTEGER;
  text_pool[text_alloc + 1] = i;
  return (text_alloc += 2) - 2;
}

text escape_text(text t, bool for_printf) {
  if (text_alloc + 3 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");

  text_pool[text_alloc] = TEXT_ESCAPED;
  text_pool[text_alloc + 1] = t;
  text_pool[text_alloc + 2] = for_printf;
  return (text_alloc += 3) - 3;
}

text string_concat(text t1, text t2) {
  if (text_alloc + 4 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_TREE;
  text_pool[text_alloc + 1] = 2;
  text_pool[text_alloc + 2] = t1;
  text_pool[text_alloc + 3] = t2;
  return (text_alloc += 4) - 4;
}

text string_concat3(text t1, text t2, text t3) {
  if (text_alloc + 5 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_TREE;
  text_pool[text_alloc + 1] = 3;
  text_pool[text_alloc + 2] = t1;
  text_pool[text_alloc + 3] = t2;
  text_pool[text_alloc + 4] = t3;
  return (text_alloc += 5) - 5;
}

text string_concat4(text t1, text t2, text t3, text t4) {
  if (text_alloc + 6 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_TREE;
  text_pool[text_alloc + 1] = 4;
  text_pool[text_alloc + 2] = t1;
  text_pool[text_alloc + 3] = t2;
  text_pool[text_alloc + 4] = t3;
  text_pool[text_alloc + 5] = t4;
  return (text_alloc += 6) - 6;
}

text string_concat5(text t1, text t2, text t3, text t4, text t5) {
  if (text_alloc + 7 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_TREE;
  text_pool[text_alloc + 1] = 5;
  text_pool[text_alloc + 2] = t1;
  text_pool[text_alloc + 3] = t2;
  text_pool[text_alloc + 4] = t3;
  text_pool[text_alloc + 5] = t4;
  text_pool[text_alloc + 6] = t5;
  return (text_alloc += 7) - 7;
}

// TODO: All strings passed to wrap_str are literals, so we should just store
// a reference to the literal instead of copying it.
text wrap_str(char *s) {
  int i = 0;
  int result = text_alloc;

  text_pool[result] = TEXT_TREE;
  text_alloc += 2;
  while (s[i] != 0) {
    text_pool[text_alloc] = -s[i];
    text_alloc += 1;
    i += 1;
  }

  text_pool[result + 1] = i;

  return result;
}

text wrap_str_pool(int s) {
  if (text_alloc + 3 >= TEXT_POOL_SIZE) fatal_error("string tree pool overflow");
  text_pool[text_alloc] = TEXT_FROM_POOL;
  text_pool[text_alloc + 1] = s;
  return (text_alloc += 2) - 2;
}

text concatenate_strings_with(text t1, text t2, text sep) {
  if (t1 == 0) return t2;
  if (t2 == 0) return t1;
  return string_concat3(t1, sep, t2);
}

void print_escaped_char(char c, int for_printf) {
  // C escape sequences
  if      (c == '\a') { putchar('\\');  putchar('a'); }
  else if (c == '\b') { putchar('\\');  putchar('b'); }
  else if (c == '\f') { putchar('\\');  putchar('f'); }
  else if (c == '\n') { putchar('\\');  putchar('n'); }
  else if (c == '\r') { putchar('\\');  putchar('r'); }
  else if (c == '\t') { putchar('\\');  putchar('t'); }
  else if (c == '\v') { putchar('\\');  putchar('v'); }
  /* backslashes are escaped twice, first by the shell and then by def_str */
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

void print_escaped_string(char *s, int for_printf) {
  int i = 0;
  while (s[i] != 0) {
    print_escaped_char(s[i], for_printf);
    i += 1;
  }
}

void print_escaped_text(text t, bool for_printf) {
  int i;

  if (t == 0) return;

  if (t < 0) { /* it's a character */
    print_escaped_char(-t, for_printf);
  } else if (text_pool[t] == TEXT_TREE) {
    i = 0;
    while (i < text_pool[t + 1]) {
      if (text_pool[t + i + 2] < 0) {
        print_escaped_char(-text_pool[t + i + 2], for_printf);
      } else {
        print_escaped_text(text_pool[t + i + 2], for_printf);
      }
      i += 1;
    }
  } else if (text_pool[t] == TEXT_INTEGER) {
    putint(text_pool[t + 1]);
  } else if ( text_pool[t] == TEXT_FROM_POOL) {
    print_escaped_string(string_pool + text_pool[t + 1], for_printf);
  } else if (text_pool[t] == TEXT_ESCAPED) {
    fatal_error("Cannot escape a string that is already escaped");
  } else {
    printf("\nt=%d %d\n", t, text_pool[t]);
    fatal_error("print_escaped_text: unexpected string tree node");
  }
}

void print_text(text t) {
  int i;

  if (t == 0) return;

  if (t < 0) { /* it's a character */
    putchar(-t);
  } else if (text_pool[t] == TEXT_TREE) {
    i = 0;
    while (i < text_pool[t + 1]) {
      if (text_pool[t + i + 2] < 0) {
        putchar(-text_pool[t + i + 2]);
      } else {
        print_text(text_pool[t + i + 2]);
      }
      i += 1;
    }
  } else if (text_pool[t] == TEXT_INTEGER) {
    putint(text_pool[t + 1]);
  } else if (text_pool[t] == TEXT_FROM_POOL) {
    putstr(string_pool + text_pool[t + 1]);
  } else if (text_pool[t] == TEXT_ESCAPED) {
    print_escaped_text(text_pool[t + 1], text_pool[t + 2]);
  } else {
    printf("\nt=%d %d\n", t, text_pool[t]);
    fatal_error("print_text: unexpected string tree node");
  }
}

/* Codegen context */

#define GLO_DECL_SIZE 100000
text glo_decls[GLO_DECL_SIZE];  /* Generated code */
int glo_decl_ix = 0;            /* Index of last generated line of code  */
int nest_level = 0;             /* Current level of indentation */
int in_tail_position = false;   /* Is the current statement in tail position? */
int in_block_head_position = false;  /* Is the current statement in head position for the current block? */
int loop_nesting_level = 0;     /* Number of loops surrounding the current statement */
int loop_end_actions_start = 0; /* Start position of declarations for the last action in a for loop */
int loop_end_actions_end = 0;   /* End position of declarations for the last action in a for loop */
ast local_env = 0;              /* List of local variables */
ast local_env_size = 0;         /* Size of the environment */
int gensym_ix = 0;              /* Counter for fresh_ident */
int fun_gensym_ix = 0;          /* Maximum value of gensym_ix for the current function */
int max_gensym_ix = 0;          /* Maximum value of gensym_ix for all functions */
int string_counter = 0;         /* Counter for string literals */
#define CHARACTERS_BITFIELD_SIZE 16
int characters_useds[16];       /* Characters used in string literals. Bitfield, each int stores 16 bits, so 16 ints in total */
bool any_character_used = false; /* If any character is used */
ast rest_loc_var_fixups = 0;    /* rest_loc_vars call to fixup after compiling a function */
bool main_defined = false;      /* If the main function is defined */
bool main_returns = false;      /* If the main function returns a value */
bool top_level_stmt = true;     /* If the current statement is at the top level */

// Internal identifier node types. These
int IDENTIFIER_INTERNAL = 600;
int IDENTIFIER_STRING = 601;
int IDENTIFIER_DOLLAR = 602;
int IDENTIFIER_EMPTY = 603;

// Node type for local variable nodes
int LOCAL_VAR = 0;

// Kind of variable (local or function parameter)
int KIND_LOCAL = 0;
int KIND_PARAM = 1;

void init_comp_context() {
  int i = 0;
  /* Initialize characters_useds table */
  while (i < 16) {
    characters_useds[i] = 0;
    i += 1;
  }
}

void append_glo_decl(text decl) {
  glo_decls[glo_decl_ix] = nest_level;
  glo_decls[glo_decl_ix + 1] = 1; /* If it's active or not. Used by undo_glo_decls and replay_glo_decls */
  glo_decls[glo_decl_ix + 2] = decl;
  glo_decl_ix += 3;
}

int append_glo_decl_fixup() {
  glo_decls[glo_decl_ix] = nest_level;
  glo_decls[glo_decl_ix + 1] = 1; /* If it's active or not. Used by undo_glo_decls and replay_glo_decls */
  glo_decls[glo_decl_ix + 2] = -1;
  glo_decl_ix += 3;
  return glo_decl_ix - 3;
}

void fixup_glo_decl(int fixup_ix, text decl) {
  if (glo_decls[fixup_ix + 2] != -1)
    fatal_error("fixup_glo_decl: invalid fixup");

  glo_decls[fixup_ix + 2] = decl;
}

/*
  Remove the n last declarations by decrementing the active field.
  A non-positive active value means that the declaration is active,
  A 0 value means that the declaration was unset once.
  A negative value means that the declaration was unset multiple times.
  Because undone declarations are generally replayed, declarations with negative
  values are ignored when replayed since they have already been replayed before.
  This is useful to compile some code at a different time than it is used.
*/
void undo_glo_decls(int start) {
  while (start < glo_decl_ix) {
    glo_decls[start + 1] -= 1; /* To support nested undone declarations */
    start += 3;
  }
}

/*
  Replay the declarations betwee start and end. Replayed declarations must first
  be undone with undo_glo_decls.
  The reindent parameter controls if the declarations should be replayed at the
  current nest level or at the nest level when they were added.
*/
void replay_glo_decls(int start, int end, int reindent) {
  while (start < end) {
    if (glo_decls[start + 1] == 0) { /* Skip inactive declarations that are at the current level */
      append_glo_decl(glo_decls[start + 2]);
      if (!reindent) glo_decls[glo_decl_ix - 3] = glo_decls[start]; /* Replace nest_level */
    }
    start += 3;
  }
}

text replay_glo_decls_inline(int start, int end) {
  text res = 0;
  while (start < end) {
    if (glo_decls[start + 1] == 0) { /* Skip inactive declarations */
      res = concatenate_strings_with(res, glo_decls[start + 2], wrap_str("; "));
    }
    start += 3;
  }
  if (res != 0) { res = string_concat(res, wrap_str("; ")); }

  return res;
}

void print_glo_decls() {
  int i = 0;
  int level;
  while (i < glo_decl_ix) {
    if (glo_decls[i + 1] == 1) { /* Skip inactive declarations */
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

ast find_var_in_local_env(ast ident_tok) {
  ast env = local_env;
  ast var;
  while (env != 0) {
    var = get_child(env, 0);
    /* Because identifier tokens are unique, we can compare their address to check if they are the same. */
    if (get_child(var, 0) == ident_tok) return var;

    env = get_child(env, 1);
  }
  return -1;
}

/*
  Similar to env_var, but doesn't use the local environment and assumes that the
  identifier is internal or global. This is faster than env_var when we know that
  the variable is not local.
*/
text format_special_var(ast ident, ast prefixed_with_dollar) {
  int op = get_op(ident);
  if (op == IDENTIFIER_INTERNAL) {
    return string_concat(wrap_str("__t"), get_val(ident));
  } else if (op == IDENTIFIER_STRING) {
    return string_concat(wrap_str("__str_"), get_val(ident));
  } else if (op == IDENTIFIER_DOLLAR) {
    if (prefixed_with_dollar) {
      if (get_val(ident) <= 9) {
        return wrap_int(get_val(ident));
      } else {
        return string_concat3(wrap_char('{'), wrap_int(get_val(ident)), wrap_char('}'));
      }
    } else {
      if (get_val(ident) <= 9) {
        return string_concat(wrap_char('$'), wrap_int(get_val(ident)));
      } else {
        return string_concat3(wrap_str("${"), wrap_int(get_val(ident)), wrap_char('}'));
      }
    }
  } else if (op == IDENTIFIER_EMPTY) {
    return wrap_str("__");
  } else {
    printf("op=%d %c", op, op);
    fatal_error("format_special_var: unknown identifier type");
    return 0;
  }
}

text struct_member_var(ast member_name_ident) {
  return string_concat(wrap_str("__"), wrap_str_pool(get_val(get_val(member_name_ident))));
}

text struct_sizeof_var(ast struct_name_ident) {
  return string_concat(wrap_str("__sizeof__"), wrap_str_pool(get_val(get_val(struct_name_ident))));
}

text global_var(ast ident_tok) {
  return string_concat(wrap_char('_'), wrap_str_pool(get_val(ident_tok)));
}

text env_var_with_prefix(ast ident, ast prefixed_with_dollar) {
  ast var;
  text res;

  if (get_op(ident) == IDENTIFIER) {
    var = find_var_in_local_env(get_val(ident));
    if (var != -1) {
      if (get_child(var, 2) == KIND_PARAM AND get_child(var, 3)) {
        res = wrap_int(get_child(var, 1));
        if (!prefixed_with_dollar) res = string_concat(wrap_char('$'), res);
      } else {
        if (get_val(ident) == ARGV_ID) {
          res = wrap_str("argv_"); //
        } else {
          res = wrap_str_pool(get_val(get_val(ident)));
        }
      }
    } else {
      res = global_var(get_val(ident));
    }
  } else {
    res = format_special_var(ident, prefixed_with_dollar);
  }

  return res;
}

text env_var(ast ident) {
  return env_var_with_prefix(ident, false);
}

text function_name(int ident_tok) {
  return string_concat(wrap_char('_'), wrap_str_pool(get_val(ident_tok)));
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

/* TODO: Reuse identifier for strings used in multiple places */
ast fresh_string_ident() {
  string_counter += 1;
  return new_ast0(IDENTIFIER_STRING, wrap_int(string_counter - 1));
}

/*
The local environment is a list of variables represented using ',' nodes.
A variable is a LOCAL_VAR node with 4 children:
  1. Ident
  2. Position of the variable in the shell environment ($1, $2, ...)
  3. Kind of variable (function param or local var)
  4. Constant: if the variable is never assigned to
*/
void add_var_to_local_env(ast ident_tok, int position, int kind) {
  ast var;
  /* Check if the variable is not in env. This should always do nothing */
  if (find_var_in_local_env(ident_tok) != -1) {
    fatal_error("add_var_to_local_env: variable already in local environment");
  }

  /*
    The var is not part of the environment, so we add it.
    Variables start as constant, and are marked as mutable by mark_mutable_variables_body.
  */
#ifdef OPTIMIZE_CONSTANT_PARAM
  var = new_ast4(LOCAL_VAR, ident_tok, position, kind, true);
#else
  var = new_ast4(LOCAL_VAR, ident_tok, position, kind, false);
#endif
  local_env = new_ast2(',', var, local_env);
  local_env_size += 1;
}

void add_vars_to_local_env(ast lst, int position, int kind) {
  ast decls;
  ast variables;
  ast variable;
  while (lst != 0) {
    decls = get_child(lst, 0); /* VAR_DECLS node */
    variables = get_child(decls, 0); /* List of variables */
    while(variables != 0){ /* Loop through the list of variables */
      variable = get_child(variables, 0); /* Variable node */
      add_var_to_local_env(get_child(variable, 0), position, kind);
      variables = get_child(variables, 1);
      position += 1; /* Increment position */
    }
    lst = get_child(lst, 1);
  }
}

void add_fun_params_to_local_env(ast lst, int position, int kind) {
  ast decl;
  while (lst != 0) {
    decl = get_child(lst, 0);
    add_var_to_local_env(get_child(decl, 0), position, kind);
    lst = get_child(lst, 1);
    position += 1;
  }
}

void mark_variable_as_mutable(ast ident) {
  ast var;
  if (get_op(ident) == IDENTIFIER) {
    var = find_var_in_local_env(get_val(ident));
    if (var != -1) { set_child(var, 3, false); }
  }
}

int variable_is_constant_param(ast local_var) {
  if (local_var != -1 && get_child(local_var, 2) == KIND_PARAM && get_child(local_var, 3)) {
    return true;
  }
  return false;
}

/*
  Since global and internal variables are prefixed with _, we restrict the name
  of variables to not start with _. Also, because some shells treat some
  variables as special, we prevent their use. Additionally, EOF and NULL cannot
  be redefined.

  Also, the shell backend doesn't support variables with an non-reference struct
  type.
*/

void assert_var_decl_is_safe(ast variable, bool local) { /* Helper function for assert_idents_are_safe */
  ast ident_tok = get_child(variable, 0);
  char* name = string_pool + get_val(ident_tok);
  ast type = get_child(variable, 1);
  if (name[0] == '_'
  || (name[0] != '\0' && name[1] == '_' && name[2] == '\0')) { // Check for a_ variables that could conflict with character constants
    printf("%s ", name);
    fatal_error("variable name is invalid. It can't start or end with '_'.");
  }

  // IFS is a special shell variable that's overwritten by certain.
  // In zsh, writing to argv assigns to $@, so we map argv to argv_, and forbid argv_.
  // This check only applies to local variables because globals are prefixed with _.
  if (local && (ident_tok == ARGV__ID || ident_tok == IFS_ID)) {
    printf("%s ", name);
    fatal_error("variable name is invalid. It can't be 'IFS' or 'argv_'.");
  }

  // Local variables don't correspond to memory locations, and can't store
  // more than 1 number/pointer.
  if (local && (get_op(type) == '[' || (get_op(type) == STRUCT_KW AND get_val(type) == 0))) {
    printf("%s ", name);
    fatal_error("array/struct value type is not supported for shell backend. Use a reference type instead.");
  }
}

void assert_vars_are_safe(ast lst, bool local) {
  ast decls;
  ast variables;
  ast variable;
  while(lst != 0){
    if(get_op(get_child(lst, 0)) == VAR_DECLS){ /* If it's a list of declarations */
      decls = get_child(lst, 0);
      variables = get_child(decls, 0);
      while(variables != 0) { /* Loop through the list of variables */
        variable = get_child(variables, 0);
        assert_var_decl_is_safe(variable, local); /* Check the variables */
        variables = get_child(variables, 1);
      }
    } else{
      assert_var_decl_is_safe(get_child(lst, 0), local); /* Check the variable */
    }
    lst = get_child(lst, 1);
  }
}

int num_vars_to_save() {
  ast env = local_env;
  int counter = fun_gensym_ix;

  while (env != 0) {
    if (!variable_is_constant_param(get_child(env, 0))) counter += 1;
    env = get_child(env, 1);
  }

  return counter;
}

#ifdef SH_SAVE_VARS_WITH_SET
// Save the value of local variables to positional parameters
text save_local_vars(int params_count) {
  ast env = local_env;
  ast local_var;
  ast ident;
  text res = 0;
  int counter = fun_gensym_ix;

  if (num_vars_to_save() == 0) return 0;

  runtime_use_local_vars = true;

  while (env != 0) {
    local_var = get_child(env, 0);
    env = get_child(env, 1);
    if (variable_is_constant_param(local_var)) continue;
    ident = new_ast0(IDENTIFIER, get_child(local_var, 0));
    res = concatenate_strings_with(string_concat(wrap_char('$'), env_var_with_prefix(ident, true)), res, wrap_char(' '));
  }

  while (counter > 0) {
    ident = new_ast0(IDENTIFIER_INTERNAL, wrap_int(fun_gensym_ix - counter + 1));
    res = concatenate_strings_with(res, string_concat(wrap_char('$'), env_var_with_prefix(ident, true)), wrap_char(' '));
    counter -= 1;
  }

  return string_concat(wrap_str("set $@ "), res);
}

// Restore the previous value of local variables from positional parameters
text restore_local_vars(int params_count) {
  ast env = local_env;
  ast local_var;
  ast ident;
  // Position of the saved local vars, starting from 0
  int local_var_pos = 0;
  text res = 0;
  int counter = fun_gensym_ix;
  // Number of non-constant variables in the environment.
  // Used to account for traversal of local env in reverse order.
  int env_non_cst_size = 0;

  if (num_vars_to_save() == 0) return 0;

  runtime_use_local_vars = true;

  while (env != 0) {
    local_var = get_child(env, 0);
    env = get_child(env, 1);
    if(variable_is_constant_param(local_var)) continue;
    env_non_cst_size += 1;
  }

  env = local_env;

  while (env != 0) {
    local_var = get_child(env, 0);
    env = get_child(env, 1);
    if (variable_is_constant_param(local_var)) continue;
    ident = new_ast0(IDENTIFIER, get_child(local_var, 0));
    res = concatenate_strings_with(string_concat5(wrap_str("$(("), env_var_with_prefix(ident, true), wrap_str(" = $"), format_special_var(new_ast0(IDENTIFIER_DOLLAR, params_count + env_non_cst_size - local_var_pos), true), wrap_str("))")), res, wrap_char(' '));
    local_var_pos += 1;
  }

  while (counter > 0) {
    ident = new_ast0(IDENTIFIER_INTERNAL, wrap_int(fun_gensym_ix - counter + 1));
    res = concatenate_strings_with(res, string_concat5(wrap_str("$(("), env_var_with_prefix(ident, true), wrap_str(" = $"), format_special_var(new_ast0(IDENTIFIER_DOLLAR, params_count + local_var_pos + 1), true), wrap_str("))")), wrap_char(' '));
    local_var_pos += 1;
    counter -= 1;
  }

  return string_concat3(wrap_str(": $((__tmp = $1)) "), res, wrap_str(" $(($1 = __tmp))"));
}

#else

#ifdef SH_INITIALIZE_PARAMS_WITH_LET
// Save the value of local variables to positional parameters
text let_params(int params) {
  ast ident;
  ast local_var;
  text res = 0;
  int params_ix = 2;

  if (num_vars_to_save() == 0) return 0;

  runtime_use_local_vars = true;

  while (params != 0) {
    local_var = find_var_in_local_env(get_child(get_child(params, 0), 0));
    if (!variable_is_constant_param(local_var)) {
      ident = new_ast0(IDENTIFIER, get_child(get_child(params, 0), 0));
      res = concatenate_strings_with(res, string_concat4(wrap_str("let "), env_var_with_prefix(ident, false), wrap_char(' '), format_special_var(new_ast0(IDENTIFIER_DOLLAR, params_ix), false)), wrap_str("; "));
    }
    params = get_child(params, 1);
    params_ix += 1;
  }

  return res;
}
#endif

text save_local_vars(int params_count) {
  ast env = local_env;
  ast local_var;
  ast ident;
  text res = 0;
  int counter = fun_gensym_ix;

  if (num_vars_to_save() == 0) return 0;

  runtime_use_local_vars = true;

  while (counter > 0) {
    ident = new_ast0(IDENTIFIER_INTERNAL, wrap_int(counter));
    res = concatenate_strings_with(string_concat(wrap_str("let "), format_special_var(ident, true)), res, wrap_str("; "));
    counter -= 1;
  }

  while (env != 0) {
    local_var = get_child(env, 0);

#ifdef SH_INITIALIZE_PARAMS_WITH_LET
    if (local_var != -1 && get_child(local_var, 2) != KIND_PARAM) { // Skip params
#else
    /* Constant function parameters are assigned to $1, $2, ... and don't need to be saved */
    if (!variable_is_constant_param(local_var)) {
#endif
      ident = new_ast0(IDENTIFIER, get_child(local_var, 0));
      res = concatenate_strings_with(string_concat(wrap_str("let "), env_var_with_prefix(ident, true)), res, wrap_str("; "));
    }

    env = get_child(env, 1);
  }

  return res;
}

/*
  The only difference between save_local_vars and restore_local_vars is the
  order of the arguments and the call to unsave_vars instead of save_vars.
*/
text restore_local_vars(int params_count) {
  ast env = local_env;
  ast local_var;
  ast ident;
  text res = 0;
  int counter = fun_gensym_ix;

  if (num_vars_to_save() == 0) return 0;

  runtime_use_local_vars = true;

  while (counter > 0) {
    ident = new_ast0(IDENTIFIER_INTERNAL, wrap_int(counter));
    res = concatenate_strings_with(res, format_special_var(ident, false), wrap_char(' '));
    counter -= 1;
  }

  while (env != 0) {
    local_var = get_child(env, 0);

    /* Constant function parameters are assigned to $1, $2, ... and don't need to be saved */
    if (!variable_is_constant_param(local_var)) {
      ident = new_ast0(IDENTIFIER, get_child(local_var, 0));
      res = concatenate_strings_with(res, env_var(ident), wrap_char(' '));
    }

    env = get_child(env, 1);
  }

  return string_concat(wrap_str("endlet $1 "), res);
}
#endif

text op_to_str(int op) {
  if      (op < 256)         return string_concat3(wrap_char(' '), wrap_char(op), wrap_char(' '));
  else if (op == AMP_AMP)    return wrap_str(" && ");
  else if (op == AMP_EQ)     return wrap_str(" &= ");
  else if (op == BAR_BAR)    return wrap_str(" || ");
  else if (op == BAR_EQ)     return wrap_str(" |= ");
  else if (op == CARET_EQ)   return wrap_str(" ^= ");
  else if (op == EQ_EQ)      return wrap_str(" == ");
  else if (op == GT_EQ)      return wrap_str(" >= ");
  else if (op == LSHIFT_EQ)  return wrap_str(" <<= ");
  else if (op == LT_EQ)      return wrap_str(" <= ");
  else if (op == LSHIFT)     return wrap_str(" << ");
  else if (op == MINUS_EQ)   return wrap_str(" -= ");
  else if (op == EXCL_EQ)    return wrap_str(" != ");
  else if (op == PERCENT_EQ) return wrap_str(" %= ");
  else if (op == PLUS_EQ)    return wrap_str(" += ");
  else if (op == RSHIFT_EQ)  return wrap_str(" >>= ");
  else if (op == RSHIFT)     return wrap_str(" >> ");
  else if (op == SLASH_EQ)   return wrap_str(" /= ");
  else if (op == STAR_EQ)    return wrap_str(" *= ");
  else {
    printf("op=%d %c\n", op, op);
    fatal_error("op_to_str: unexpected operator");
    return 0;
  }
}

/*
  Similar to op_to_str, but returns the shell test operand instead of the C-style operands.
*/
text test_op_to_str(int op) {
  // For == and !=, because integers are stored as strings in most shells, the
  // conversion to int can be avoided by comparing the strings instead of using
  // -eq and -ne.
  if      (op == EQ_EQ)      return wrap_str(" = ");
  else if (op == EXCL_EQ)    return wrap_str(" != ");
  else if (op == '<')        return wrap_str(" -lt ");
  else if (op == '>')        return wrap_str(" -gt ");
  else if (op == LT_EQ)      return wrap_str(" -le ");
  else if (op == GT_EQ)      return wrap_str(" -ge ");
  else {
    printf("op=%d %c\n", op, op);
    fatal_error("test_op_to_str: unexpected operator");
    return 0;
  }
}

text character_ident(int c) {
  /* Mark character as used */
  characters_useds[c / CHARACTERS_BITFIELD_SIZE] |= 1 << (c % CHARACTERS_BITFIELD_SIZE);
  any_character_used = true;

  if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || ('0' <= c && c <= '9')) {
    return string_concat5(wrap_char('_'), wrap_char('_'), wrap_char(c), wrap_char('_'), wrap_char('_'));
  } else {
    if      (c == '\0') return wrap_str("__NUL__");
    else if (c == '\n') return wrap_str("__NEWLINE__");
    else if (c == ' ')  return wrap_str("__SPACE__");
    else if (c == '!')  return wrap_str("__EXCL__");
    else if (c == '"')  return wrap_str("__DQUOTE__");
    else if (c == '#')  return wrap_str("__SHARP__");
    else if (c == '$')  return wrap_str("__DOLLAR__");
    else if (c == '%')  return wrap_str("__PERCENT__");
    else if (c == '&')  return wrap_str("__AMP__");
    else if (c == '\'') return wrap_str("__QUOTE__");
    else if (c == '(')  return wrap_str("__LPAREN__");
    else if (c == ')')  return wrap_str("__RPAREN__");
    else if (c == '*')  return wrap_str("__STAR__");
    else if (c == '+')  return wrap_str("__PLUS__");
    else if (c == ',')  return wrap_str("__COMMA__");
    else if (c == '-')  return wrap_str("__MINUS__");
    else if (c == '.')  return wrap_str("__PERIOD__");
    else if (c == '/')  return wrap_str("__SLASH__");
    else if (c == ':')  return wrap_str("__COLON__");
    else if (c == ';')  return wrap_str("__SEMICOLON__");
    else if (c == '<')  return wrap_str("__LT__");
    else if (c == '=')  return wrap_str("__EQ__");
    else if (c == '>')  return wrap_str("__GT__");
    else if (c == '?')  return wrap_str("__QUESTION__");
    else if (c == '@')  return wrap_str("__AT__");
    else if (c == '^')  return wrap_str("__CARET__");
    else if (c == '[')  return wrap_str("__LBRACK__");
    else if (c == '\\') return wrap_str("__BACKSLASH__");
    else if (c == ']')  return wrap_str("__RBRACK__");
    else if (c == '_')  return wrap_str("__UNDERSCORE__");
    else if (c == '`')  return wrap_str("__BACKTICK__");
    else if (c == '{')  return wrap_str("__LBRACE__");
    else if (c == '|')  return wrap_str("__BAR__");
    else if (c == '}')  return wrap_str("__RBRACE__");
    else if (c == '~')  return wrap_str("__TILDE__");
    else if (c == '\a') return wrap_str("__ALARM__");
    else if (c == '\b') return wrap_str("__BACKSPACE__");
    else if (c == '\f') return wrap_str("__PAGE__");
    else if (c == '\r') return wrap_str("__RET__");
    else if (c == '\t') return wrap_str("__TAB__");
    else if (c == '\v') return wrap_str("__VTAB__");
    else { fatal_error("Unknown character"); return 0; }
  }
}

ast replaced_fun_calls = 0;
ast replaced_fun_calls_tail = 0;
ast conditional_fun_calls = 0;
ast conditional_fun_calls_tail = 0;
ast literals_inits = 0;
int executes_conditionally = 0;
int contains_side_effects = 0;

/*
  We can't have function calls and other side effects in $(( ... )), so we need to handle them separately.
  For unconditional function calls, they are replaced with unique identifiers and returned as a list with their new identifiers.
  For pre/post-increments/decrements, we map them to a pre-side-effects and replace with the corresponding operation.
  Note that pre/post-increments/decrements of function calls are not supported.
*/
ast handle_side_effects_go(ast node, int executes_conditionally) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int start_gensym_ix;
  ast sub1;
  ast sub2;
  ast previous_conditional_fun_calls;
  ast left_conditional_fun_calls;
  ast right_conditional_fun_calls;
  ast new_tail;

  if (nb_children == 0) {
    if (op == IDENTIFIER OR op == IDENTIFIER_INTERNAL OR op == IDENTIFIER_STRING OR op == IDENTIFIER_DOLLAR OR op == INTEGER OR op == CHARACTER) {
      return node;
    } else if (op == STRING) {
      /* We must initialize strings before the expression */
      sub1 = fresh_string_ident();
      literals_inits = new_ast2(',', new_ast2(',', sub1, get_val(node)), literals_inits);
      return sub1;
    } else {
      printf("handle_side_effects_go: op=%d %c", op, op);
      fatal_error("unexpected operator");
      return 0;
    }
  } else if (nb_children == 1) {
    if ((op == '&') OR (op == '*') OR (op == '+') OR (op == '-') OR (op == '~') OR (op == '!')) {
      /* TODO: Reuse ast node? */
      return new_ast1(op, handle_side_effects_go(get_child(node, 0), executes_conditionally));
    } else if (op == PLUS_PLUS_PRE || op == MINUS_MINUS_PRE || op == PLUS_PLUS_POST || op == MINUS_MINUS_POST) {
      contains_side_effects = true;
      return new_ast1(op, handle_side_effects_go(get_child(node, 0), executes_conditionally));
    } else if (op == SIZEOF_KW) {
      return node; // sizeof is a compile-time operator
    } else {
      printf("1: op=%d %c", op, op);
      fatal_error("unexpected operator");
      return 0;
    }
  } else if (nb_children == 2) {
    if (op == '(') { /* Function call */
      sub1 = fresh_ident(); /* Unique identifier for the function call */

      start_gensym_ix = gensym_ix;

      /* Traverse the arguments and replace them with the result of handle_side_effects_go */
      sub2 = get_child(node, 1);
      if (sub2 != 0) { /* Check if not an empty list */
        if (get_op(sub2) == ',') {
          while (get_op(sub2) == ',') {
            set_child(sub2, 0, handle_side_effects_go(get_child(sub2, 0), executes_conditionally));
            sub2 = get_child(sub2, 1);
          }
        } else { /* sub2 is the first argument, not wrapped in a cons cell */
          sub2 = handle_side_effects_go(sub2, executes_conditionally);
          set_child(node, 1, sub2);
        }
      }

      /*
        All the temporary variables used for the function parameters can be
        reused after the function call, so resetting the gensym counter.
      */
      gensym_ix = start_gensym_ix;

      new_tail = new_ast2(',', sub1, node);
      new_tail = new_ast2(',', new_tail, 0);
      if (executes_conditionally) {
        if (conditional_fun_calls == 0) { conditional_fun_calls = new_tail; }
        else { set_child(conditional_fun_calls_tail, 1, new_tail); }
        conditional_fun_calls_tail = new_tail;
      }
      else {
        if (replaced_fun_calls == 0) { replaced_fun_calls = new_tail; }
        else { set_child(replaced_fun_calls_tail, 1, new_tail); }
        replaced_fun_calls_tail = new_tail;
      }

      return sub1;
    } else if ( (op == '&') OR (op == '|') OR (op == '<') OR (op == '>') OR (op == '+') OR (op == '-') OR (op == '*') OR (op == '/')
      OR (op == '%') OR (op == '^') OR (op == ',') OR (op == EQ_EQ) OR (op == EXCL_EQ) OR (op == LT_EQ) OR (op == GT_EQ) OR (op == LSHIFT) OR (op == RSHIFT) OR (op == '=') OR (op == '[')
      OR (op == '.') OR (op == ARROW) ) {
      /* We can't place handle_side_effects_go directly in new_ast2 call because six-cc creates a global variable that gets overwritten in the other handle_side_effects_go calls */
      sub1 = handle_side_effects_go(get_child(node, 0), executes_conditionally);
      sub2 = handle_side_effects_go(get_child(node, 1), executes_conditionally); /* We could inline that one since the assignment to the global variable is done after the last handle_side_effects_go call */
      return new_ast2(op, sub1, sub2);
    } else if ((op == AMP_EQ) OR (op == BAR_EQ) OR (op == CARET_EQ) OR (op == LSHIFT_EQ) OR (op == MINUS_EQ) OR (op == PERCENT_EQ) OR (op == PLUS_EQ) OR (op == RSHIFT_EQ) OR (op == SLASH_EQ) OR (op == STAR_EQ)) {
      /* Just like previous case, except that we update contains_side_effects */
      contains_side_effects = true;
      sub1 = handle_side_effects_go(get_child(node, 0), executes_conditionally);
      sub2 = handle_side_effects_go(get_child(node, 1), executes_conditionally); /* We could inline that one since the assignment to the global variable is done after the last handle_side_effects_go call */
      return new_ast2(op, sub1, sub2);
    } else if ((op == AMP_AMP) OR (op == BAR_BAR)) {
      previous_conditional_fun_calls = conditional_fun_calls;
      conditional_fun_calls = 0;
      /*
        The left side is always executed, unless the whole expression is executed conditionally.
        We could compile it as always executed, but it makes the Shell code less regular so we compile it conditionally.
      */
      sub1 = handle_side_effects_go(get_child(node, 0), true);
      left_conditional_fun_calls = conditional_fun_calls;
      conditional_fun_calls = 0;
      sub2 = handle_side_effects_go(get_child(node, 1), true);
      right_conditional_fun_calls = conditional_fun_calls;
      conditional_fun_calls = previous_conditional_fun_calls;
      return new_ast4(op, sub1, sub2, left_conditional_fun_calls, right_conditional_fun_calls);
    } else if (op == CAST) {
      return new_ast2(CAST, get_child(node, 0), handle_side_effects_go(get_child(node, 1), executes_conditionally));
    } else {
      printf("2: op=%d %c", op, op);
      fatal_error("unexpected operator");
      return 0;
    }
  } else if (nb_children == 3) {
    if (op == '?') {
      previous_conditional_fun_calls = conditional_fun_calls;
      conditional_fun_calls = 0;
      sub1 = handle_side_effects_go(get_child(node, 1), true);
      left_conditional_fun_calls = conditional_fun_calls;
      conditional_fun_calls = 0;
      sub2 = handle_side_effects_go(get_child(node, 2), true);
      right_conditional_fun_calls = conditional_fun_calls;
      if (left_conditional_fun_calls != 0 OR right_conditional_fun_calls != 0) {
        fatal_error("Conditional function calls in ternary operator not allowed");
      }

      return new_ast3('?', handle_side_effects_go(get_child(node, 0), executes_conditionally), sub1, sub2);
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

void comp_defstr(ast ident, int string_pool_str) {
  if (top_level_stmt) {
    // If defstr is used at the top level, it needs to be included beforehand
    runtime_defstr();
  } else {
    runtime_use_defstr = true;
  }

  append_glo_decl(string_concat5( wrap_str("defstr ")
                                , format_special_var(ident, false)
                                , wrap_str(" \"")
                                , escape_text(wrap_str_pool(string_pool_str), false)
                                , wrap_char('\"')));
}

int RVALUE_CTX_BASE = 0;
int RVALUE_CTX_ARITH_EXPANSION = 1; /* Like base context, except that we're already in $(( ... )) */
int RVALUE_CTX_TEST = 2;
int RVALUE_CTX_TEST_ELSEIF = 3;

text with_prefixed_side_effects(ast test_side_effects, text code) {

  text test_side_effects_code = 0;

  while (test_side_effects != 0) {
    test_side_effects_code =
      string_concat3(test_side_effects_code,
                     comp_fun_call_code(get_child(get_child(test_side_effects, 0), 1), get_child(get_child(test_side_effects, 0), 0)),
                     wrap_str("; "));
    test_side_effects = get_child(test_side_effects, 1);
  }
  if (test_side_effects_code != 0) {
    return string_concat4(wrap_str("{ "), test_side_effects_code, code, wrap_str("; }"));
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

/*
  Wrap code in $((...)) if it's not already and if it's already in $(( )), wrap
  it in parentheses if parens_otherwise is true. If it's not in an arithmetic
  expansion and we're compiling tests, we also add the test condition to make it
  a valid test.
*/
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
    return with_prefixed_side_effects(test_side_effects, string_concat3(wrap_str("[ $(("), code, wrap_str(")) != 0 ]")));
  } else {
    return string_concat3(wrap_str("$(("), code, wrap_str("))"));
  }
}

/*
  Used to supports the case `if/while (c) { ... }`, where c is a variable or a literal.
  This is otherwise handled by wrap-if-needed, but we don't want to wrap in $(( ... )) here.
*/
text wrap_in_condition_if_needed(int context, ast test_side_effects, text code) {
  if (context == RVALUE_CTX_TEST) {
    return with_prefixed_side_effects(test_side_effects, string_concat3(wrap_str("[ "), code, wrap_str(" != 0 ]")));
  } else {
    return code;
  }
}

text comp_rvalue_go(ast node, int context, ast test_side_effects, int outer_op) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  text sub1;
  text sub2;
  text sub3;

  if (nb_children == 0) {
    if (op == INTEGER) {
      return wrap_in_condition_if_needed(context, test_side_effects, wrap_int(-get_val(node)));
    } else if (op == CHARACTER) {
#ifdef SH_INLINE_CHAR_LITERAL
      return wrap_in_condition_if_needed(context, test_side_effects, wrap_int(get_val(node)));
#else
      if (context == RVALUE_CTX_ARITH_EXPANSION) {
        return character_ident(get_val(node));
      } else {
        return wrap_in_condition_if_needed(context, test_side_effects, string_concat(wrap_char('$'), character_ident(get_val(node))));
      }
#endif
    } else if (op == IDENTIFIER OR op == IDENTIFIER_INTERNAL OR op == IDENTIFIER_STRING OR op == IDENTIFIER_DOLLAR) {
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
      /*
        Setting context to RVALUE_CTX_BASE even if it's wrapped in $(( ... )) because we
        need another layer of wrapping if it's a complex expression, i.e. not a
        literal or a variable.
      */
      sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_BASE, 0, op);
      return wrap_if_needed(false, context, test_side_effects, string_concat(wrap_char('_'), sub1), outer_op, op);
    } else if (op == '+') {
      /* +x is equivalent to x */
      return comp_rvalue_go(get_child(node, 0), context, test_side_effects, op);
    } else if (op == '-') {
      /*
        Check if the rest of ast is a literal, if so directly return the negated value.
        Note: I think this can be simplified by not wrapped in () in the else case.
      */
      if (get_op(get_child(node, 0)) == INTEGER) {
        return wrap_in_condition_if_needed(context, test_side_effects, wrap_int(get_val(get_child(node, 0))));
      } else {
        sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION, 0, op);
        return wrap_if_needed(false, context, test_side_effects, string_concat3(wrap_str("-("), sub1, wrap_char(')')), outer_op, op);
      }
    } else if (op == '~') {
      sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION, 0, op);
      return wrap_if_needed(false, context, test_side_effects, string_concat3(wrap_str("~("), sub1, wrap_char(')')), outer_op, op);
    } else if (op == '!') {
      sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION, 0, op);
      return wrap_if_needed(true, context, test_side_effects, string_concat(wrap_char('!'), sub1), outer_op, op);
    } else if (op == MINUS_MINUS_PRE) {
      sub1 = comp_lvalue(get_child(node, 0));
      return wrap_if_needed(true, context, test_side_effects, string_concat(sub1, wrap_str(" -= 1")), outer_op, op);
    } else if (op == PLUS_PLUS_PRE) {
      sub1 = comp_lvalue(get_child(node, 0));
      return wrap_if_needed(true, context, test_side_effects, string_concat(sub1, wrap_str(" += 1")), outer_op, op);
    } else if (op == MINUS_MINUS_POST) {
      sub1 = comp_lvalue(get_child(node, 0));
      return wrap_if_needed(false, context, test_side_effects,string_concat4(wrap_str("("), sub1, wrap_str(" -= 1)"), wrap_str(" + 1")), outer_op, '+');
    } else if (op == PLUS_PLUS_POST) {
      sub1 = comp_lvalue(get_child(node, 0));
      return wrap_if_needed(false, context, test_side_effects, string_concat4(wrap_str("("), sub1, wrap_str(" += 1)"), wrap_str(" - 1")), outer_op, '-');
    } else if (op == SIZEOF_KW) {
      if (get_op(get_child(node, 0)) == INT_KW
       || get_op(get_child(node, 0)) == CHAR_KW
       || get_op(get_child(node, 0)) == VOID_KW
       || get_op(get_child(node, 0)) == ENUM_KW
       || (( get_op(get_child(node, 0)) == STRUCT_KW || get_op(get_child(node, 0)) == UNION_KW)
          && get_child(get_child(node, 0), 0) >= 1)) { // If it's a pointer
        return wrap_in_condition_if_needed(context, test_side_effects, wrap_int(1));
      } else if (get_op(get_child(node, 0)) == STRUCT_KW) {
        return wrap_if_needed(false, context, test_side_effects, struct_sizeof_var(get_child(get_child(node, 0), 1)), outer_op, op);
      } else {
        fatal_error("comp_rvalue_go: sizeof is not supported for this type or expression");
      }
    } else if (op == '&') {
      return wrap_if_needed(false, context, test_side_effects, comp_lvalue_address(get_child(node, 0)), outer_op, op);
    } else {
      printf("1: op=%d %c", op, op);
      fatal_error("comp_rvalue_go: unexpected operator");
      return 0;
    }
  } else if (nb_children == 2) {
    if (op == '+' OR op == '-' OR op == '*' OR op == '/' OR op == '%' OR op == '&' OR op == '|' OR op == '^' OR op == LSHIFT OR op == RSHIFT) {
      sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION, 0, op);
      sub2 = comp_rvalue_go(get_child(node, 1), RVALUE_CTX_ARITH_EXPANSION, 0, op);
      return wrap_if_needed(true, context, test_side_effects, string_concat3(sub1, op_to_str(op), sub2), outer_op, op);
    } else if (op == '=' OR op == AMP_EQ OR op == BAR_EQ OR op == CARET_EQ OR op == LSHIFT_EQ OR op == MINUS_EQ OR op == PERCENT_EQ OR op == PLUS_EQ OR op == RSHIFT_EQ OR op == SLASH_EQ OR op == STAR_EQ) {
      sub1 = comp_lvalue(get_child(node, 0));
      sub2 = comp_rvalue_go(get_child(node, 1), RVALUE_CTX_ARITH_EXPANSION, 0, op);
      return wrap_if_needed(true, context, test_side_effects, string_concat3(sub1, op_to_str(op), sub2), outer_op, op);
    } else if (op == '[') { // array indexing
      sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION, 0, '+');
      sub2 = comp_rvalue_go(get_child(node, 1), RVALUE_CTX_ARITH_EXPANSION, 0, '+');
      return wrap_if_needed(false, context, test_side_effects, string_concat5(wrap_str("_$(("), sub1, wrap_str(" + "), sub2, wrap_str("))")), outer_op, op);
    } else if (op == ARROW) { // member access is implemented like array access
      sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION, 0, op);
      sub2 = struct_member_var(get_child(node, 1));
      return wrap_if_needed(false, context, test_side_effects, string_concat5(wrap_str("_$(("), sub1, wrap_str(" + "), sub2, wrap_str("))")), outer_op, op);
    } else if (op == EQ_EQ OR op == EXCL_EQ OR op == LT_EQ OR op == GT_EQ OR op == '<' OR op == '>') {
      if (context == RVALUE_CTX_TEST) {
        sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_BASE, 0, op);
        sub2 = comp_rvalue_go(get_child(node, 1), RVALUE_CTX_BASE, 0, op);
        return with_prefixed_side_effects(test_side_effects, string_concat5(wrap_str("[ "), sub1, test_op_to_str(op), sub2, wrap_str(" ]")));
      } else {
        sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION, 0, op);
        sub2 = comp_rvalue_go(get_child(node, 1), RVALUE_CTX_ARITH_EXPANSION, 0, op);
        return wrap_if_needed(true, context, test_side_effects, string_concat3(sub1, op_to_str(op), sub2), outer_op, op);
      }
    } else if (op == CAST) { // Casts are no-op
      return comp_rvalue_go(get_child(node, 1), context, 0, op);
    } else if (op == AMP_AMP OR op == BAR_BAR) {
      fatal_error("comp_rvalue_go: && and || should have 4 children by that point");
      return 0;
    } else {
      fatal_error("comp_rvalue_go: unknown rvalue with 2 children");
      return 0;
    }
  } else if (nb_children == 3) {
    if (op == '?') {
      sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION, 0, op);
      sub2 = comp_rvalue_go(get_child(node, 1), RVALUE_CTX_ARITH_EXPANSION, 0, op);
      sub3 = comp_rvalue_go(get_child(node, 2), RVALUE_CTX_ARITH_EXPANSION, 0, op);
      return wrap_if_needed(true, context, test_side_effects, string_concat5(sub1, op_to_str(op), sub2, wrap_str(": "), sub3), outer_op, op);
      return 0;
    } else {
      printf("op=%d %c\n", op, op);
      fatal_error("comp_rvalue_go: unknown rvalue with 3 children");
      return 0;
    }
  } else if (nb_children == 4) {
    if (op == AMP_AMP OR op == BAR_BAR) {
      /*
        Note, this could also be compiled in a single [ ] block using -a and -o,
        which I think are POSIX compliant but are deprecated.
      */
      if (context == RVALUE_CTX_TEST) {
        /*
          When compiling in a test context, && and || can be compiled to Shell's
          && and || with [ ... ] blocks.

          A notable difference between these operators in Shell and C is that in
          Shell, they have equal precedence while in C, && has higher precedence.
          This means that we need to add parenthesis that would not be needed in C.

          As a heuristic, we add parenthesis whenever the left or right side of
          the operator is a different comparison operator.
        */
        sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_TEST, get_child(node, 2), op);
        sub2 = comp_rvalue_go(get_child(node, 1), RVALUE_CTX_TEST, get_child(node, 3), op);
        if ((get_op(get_child(node, 0)) == AMP_AMP OR get_op(get_child(node, 0)) == BAR_BAR) AND get_op(get_child(node, 0)) != op) {
          sub1 = string_concat3(wrap_str("{ "), sub1, wrap_str("; }"));
        }
        if ((get_op(get_child(node, 1)) == AMP_AMP OR get_op(get_child(node, 1)) == BAR_BAR) AND get_op(get_child(node, 1)) != op) {
          sub2 = string_concat3(wrap_str("{ "), sub2, wrap_str("; }"));
        }
        return string_concat3(sub1, op_to_str(op), sub2);
      } else {
        if (test_side_effects != 0 OR get_child(node, 2) != 0 OR get_child(node, 3) != 0) {
          fatal_error("comp_rvalue_go: && and || with function calls can only be used in tests");
        }
        sub1 = comp_rvalue_go(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION, 0, op);
        sub2 = comp_rvalue_go(get_child(node, 1), RVALUE_CTX_ARITH_EXPANSION, 0, op);
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
  /* Calling comp_fun_call/comp_rvalue can overwrite replaced_fun_calls and contains_side_effects, so they are saved */
  ast replaced_fun_calls2 = replaced_fun_calls;
  int contains_side_effects2 = contains_side_effects;
  int fun_call_decl_start;
  text result;

  // Capture the start of the side effects to be able to undo them if needed
  fun_call_decl_start = glo_decl_ix;

  while (literals_inits != 0) {
    comp_defstr(get_child(get_child(literals_inits, 0), 0), get_child(get_child(literals_inits, 0), 1));
    literals_inits = get_child(literals_inits, 1);
  }

  /*
    We don't want to call defstr on every iteration, so we only capture fun
    calls, not literal initialization. That's unless it's for a elif statement,
    because the previous block is not executed so the def_str calls must be
    placed inline with the rest of the condition.
  */

  if (context != RVALUE_CTX_TEST_ELSEIF)
    fun_call_decl_start = glo_decl_ix;

  while (replaced_fun_calls2 != 0) {
    comp_fun_call(get_child(get_child(replaced_fun_calls2, 0), 1), get_child(get_child(replaced_fun_calls2, 0), 0));
    replaced_fun_calls2 = get_child(replaced_fun_calls2, 1);
  }

  /*
    When compiling a test, we place the function side effects inline with the condition.
    That way, any side effect performed in the condition of a while loop is repeated on each iteration.
    For if statements, it makes things shorter, but not always more readable.
  */
  if (context == RVALUE_CTX_TEST OR context == RVALUE_CTX_TEST_ELSEIF) {
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
  } else if (op == '[') {
    sub1 = comp_rvalue(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION);
    sub2 = comp_rvalue(get_child(node, 1), RVALUE_CTX_ARITH_EXPANSION);
    return string_concat3(sub1, wrap_str(" + "), sub2);
  } else if (op == '*') {
    return comp_rvalue(get_child(node, 0), RVALUE_CTX_BASE);
  } else if (op == ARROW) {
    sub1 = comp_rvalue(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION);
    sub2 = struct_member_var(get_child(node, 1));
    return string_concat3(sub1, wrap_str(" + "), sub2);
    return string_concat5(wrap_str("_$(("), sub1, wrap_str(" + "), sub2, wrap_str("))"));
  } else if (op == CAST) {
    return comp_lvalue_address(get_child(node, 1));
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

  if (op == IDENTIFIER OR op == IDENTIFIER_INTERNAL OR op == IDENTIFIER_STRING OR op == IDENTIFIER_EMPTY OR op == IDENTIFIER_DOLLAR) {
    return env_var(node);
  } else if (op == '[') {
    sub1 = comp_rvalue(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION);
    sub2 = comp_rvalue(get_child(node, 1), RVALUE_CTX_ARITH_EXPANSION);
    return string_concat5(wrap_str("_$(("), sub1, wrap_str(" + "), sub2, wrap_str("))"));
  } else if (op == '*') {
    sub1 = comp_rvalue(get_child(node, 0), RVALUE_CTX_BASE);
    return string_concat(wrap_char('_'), sub1);
  } else if (op == ARROW) {
    sub1 = comp_rvalue(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION);
    sub2 = struct_member_var(get_child(node, 1));
    return string_concat5(wrap_str("_$(("), sub1, wrap_str(" + "), sub2, wrap_str("))"));
    return string_concat3(wrap_str("_$(("), sub1, wrap_str("))"));
  } else if (op == CAST) {
    return comp_lvalue(get_child(node, 1));
  } else {
    printf("op=%d %c\n", op, op);
    fatal_error("comp_lvalue: unknown lvalue");
    return 0;
  }
}

text fun_call_params(ast params, int count) {
  ast param;
  text code_params = 0;

  if (params != 0 && count > 0) { /* Check if not an empty list */
    if (get_op(params) == ',') {
      while (get_op(params) == ',' && count > 0) {
        param = comp_rvalue(get_child(params, 0), RVALUE_CTX_BASE);
        code_params = concatenate_strings_with(code_params, param, wrap_char(' '));
        params = get_child(params, 1);
        count -= 1;
      }
    } else {
      code_params = comp_rvalue(params, RVALUE_CTX_BASE);
    }
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
  text res = comp_rvalue(param, RVALUE_CTX_ARITH_EXPANSION);
  ast ident;

  if (contains_side_effects) {
    ident = fresh_ident();
    append_glo_decl(string_concat4(comp_lvalue(ident), wrap_str("=$(("), res, wrap_str("))")));
    res = comp_lvalue(ident);
  } else if (get_op(param) != IDENTIFIER) {
    res = string_concat3(wrap_char('('), res, wrap_char(')'));
  }

  res =
    string_concat3(
      string_concat3(wrap_str("$(("), res, wrap_str("/64))")),
      string_concat3(wrap_str("$(("), res, wrap_str("/8%8))")),
      string_concat3(wrap_str("$(("), res, wrap_str("%8))")));

  return string_concat(wrap_str("printf \\\\"), res);
}
#endif

#ifdef SH_AVOID_PRINTF_USE
bool printf_uses_shell_format_specifiers(char* a) {
  // The supported format specifiers are those that are common between C and shell,
  // and those for which the representation is the same in both languages.
  // For now, this includes %d, %c, %x.
  // Non-literals strings cannot be passed directly to printf as they first need
  // to be unpacked, but can be passed to _puts from the runtime which can
  // handle them.
  while (*a != '\0') {
    if (*a == '%') {
      a += 1;
      if (*a != 'd' && *a != 'c' && *a != 'x' && *a != '%' && *a != 's') {
        return false; // Unsupported format specifier
      }
    }

    a += 1;
  }
  return true;
}

void printf_util_call(char *format_str, ast params, int params_count) {
  // Some shells interpret leading - as options. In that case, we add an empty "%s" argument.

  if (format_str[0] == '-') {
    append_glo_decl(string_concat4(wrap_str("printf \"%s"), escape_text(wrap_str(format_str), false), wrap_str("\" \"\" "), fun_call_params(params, params_count)));
  } else {
    append_glo_decl(string_concat4(wrap_str("printf \""), escape_text(wrap_str(format_str), false), wrap_str("\" "), fun_call_params(params, params_count)));
  }
}

// _printf pulls a lot of dependencies from the runtime. In most cases the
// format string is known at compile time, and we can avoid calling printf by
// using the shell's printf instead. This function generates a sequence of shell
// printf and put_pstr equivalent to the given printf call.
void handle_printf_call(char* format_str, ast params) {
  // The supported format specifiers are those that are common between C and shell,
  // and those for which the representation is the same in both languages.
  // For now, this includes %d, %c, %x
  // Non-literals strings are not supported because they would need to first be unpacked.
  char* format_start = format_str;
  int params_count = 0;
  ast params_start = params;

  while (*format_str != '\0') {
    if (*format_str == '%') {
      format_str += 1;
      if (*format_str == 'd' || *format_str == 'x') {
        // Keep accumulating the format string
        if (params == 0) fatal_error("Not enough parameters for printf");
        params_count += 1;
        params = get_child(params, 1);
      } else if (*format_str == 's' || *format_str == 'c') {
        // We can't pass strings to printf directly, they need to be unpacked first.
        // We do that by calling the _print_pnut_str function.

        // Generate the printf call for the format string up to this point.
        if (format_start != format_str - 1) {
          *(format_str - 1) = '\0'; // Null-terminate the format string

          printf_util_call(format_start, params_start, params_count);
          *(format_str - 1) = '%'; // Restore the format string, because it's a string from the string_pool
        }

        if (*format_str == 's') {
          runtime_use_put_pstr = true;
          append_glo_decl(string_concat(wrap_str("_put_pstr __ "), comp_rvalue(get_child(params, 0), RVALUE_CTX_BASE)));
        } else {
          append_glo_decl(comp_putchar_inline(get_child(params, 0)));
        }
        gensym_ix = 0; // We generate multiple statements, and we want to reuse the same gensym names
        format_start = format_str + 1; // skip the 's'
        params = get_child(params, 1); // skip the string parameter
        params_start = params;
      } else if (*format_str != '%') { // Do nothing for %%
        fatal_error("Unsupported format specifier");
      }
    }

    // Keep accumulating the format string
    format_str += 1;
  }

  // Dump the remaining format string
  if (format_start != format_str) {
    printf_util_call(format_start, params_start, 10000);
  }
}
#endif

text comp_fun_call_code(ast node, ast assign_to) {
  ast name = get_child(node, 0);
  ast params = get_child(node, 1);
  int name_id = get_val(name);
  text res;

#ifdef SH_AVOID_PRINTF_USE
  if (get_op(assign_to) == IDENTIFIER_EMPTY) {
    if (((name_id == PUTSTR_ID OR name_id == PUTS_ID) && params != 0 && get_op(params) == STRING) // puts("...")
      || (name_id == PRINTF_ID && params != 0 && get_op(params) == STRING)) { // printf("...")
      return string_concat3(wrap_str("printf \""), escape_text(wrap_str_pool(get_val(params)), true), wrap_str("\""));
    } else if (name_id == PRINTF_ID && params != 0 && get_op(params) == ',') {
      if (printf_uses_shell_format_specifiers(string_pool + get_val(get_child(params, 0)))) {
        handle_printf_call(string_pool + get_val(get_child(params, 0)), get_child(params, 1));
        return 0; // This generates no code. I guess we could return the last printf call?
      }
    }
#ifdef SH_INLINE_PUTCHAR
    else if (name_id == PUTCHAR_ID && params != 0 && get_op(params) != ',') { // putchar with 1 param
      return comp_putchar_inline(params);
    }
#endif
#ifdef SH_INLINE_EXIT
    else if (name_id == EXIT_ID && params != 0 && get_op(params) != ',') { // exit with 1 param
      res = comp_rvalue(params, RVALUE_CTX_BASE);
      return string_concat(wrap_str("exit "), res);
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

  return string_concat5(
    function_name(get_val(name)),
    wrap_char(' '),
    comp_lvalue(assign_to),
    wrap_char(' '),
    fun_call_params(params, 1000) // 1000 is an arbitrary large number
  );
}

void comp_fun_call(ast node, ast assign_to) {
  text res = comp_fun_call_code(node, assign_to);
  if (res)
    append_glo_decl(res);
}

void comp_assignment(ast lhs, ast rhs) {
  int lhs_op = get_op(lhs);
  if (lhs_op == IDENTIFIER OR lhs_op == '[' OR lhs_op == '*' OR lhs_op == ARROW) {
    if (get_op(rhs) == '(') {
      comp_fun_call(rhs, lhs);
    } else {
      /*
        TODO: This may need to be disabled because of arithmetic precision issues with some shells.
      */
      /* If lhs is an identifier, we generate x=$(( ... )) instead of : $(( x = ... )) */
      if (lhs_op == IDENTIFIER) {
        append_glo_decl(string_concat3(comp_lvalue(lhs), wrap_char('='), comp_rvalue(rhs, RVALUE_CTX_BASE)));
      } else {
        append_glo_decl(string_concat5(wrap_str(": $(("), comp_lvalue(lhs), wrap_str(" = "), comp_rvalue(rhs, RVALUE_CTX_ARITH_EXPANSION), wrap_str("))")));
      }
    }
  } else {
    printf("lhs_op=%d %c\n", lhs_op, lhs_op);
    fatal_error("unknown lhs");
  }
}

void comp_body(ast node) {
  int start_in_tail_position = in_tail_position;
  in_tail_position = false;
  in_block_head_position = true;

  if (node != 0) {
    while (get_op(node) == '{') {
      /* Last statement of body is in tail position if the body itself is in tail position */
      if (get_op(get_child(node, 1)) != '{') in_tail_position = start_in_tail_position;
      comp_statement(get_child(node, 0), false);
      node = get_child(node, 1);
      in_block_head_position = false;
    }
  }
}

// Return if the statement is a break or return statement, meaning that the block should be terminated.
// A switch conditional block is considered terminated if it ends with a break or return statement.
bool comp_switch_block_statement(ast node, bool start_in_tail_position) {
  if (get_op(node) == BREAK_KW) {
    return true;
  } else if (get_op(node) == RETURN_KW) {
    // A return marks the end of the conditional block, so it's in tail position
    in_tail_position = start_in_tail_position;
    comp_statement(node, false);
    return true;
  } else if (get_op(node) == CASE_KW || get_op(node) == DEFAULT_KW) {
    fatal_error("comp_statement: case must be at the beginning of a switch block, and each block must end with a break or return statement");
  } else {
    comp_statement(node, false);
    return false;
  }
}

void comp_switch(ast node) {
  int start_in_tail_position = in_tail_position;
  ast statement;
  text str;

  append_glo_decl(string_concat3(
      wrap_str("case "),
      comp_rvalue(get_child(node, 0), RVALUE_CTX_BASE),
      wrap_str(" in")
    ));

  nest_level += 1;

  node = get_child(node, 1);

  if (node == 0 || get_op(node) != '{') fatal_error("comp_statement: switch without body");

  while (get_op(node) == '{') {
    statement = get_child(node, 0);
    node = get_child(node, 1);
    if (get_op(statement) != CASE_KW AND get_op(statement) != DEFAULT_KW) {
      fatal_error("comp_statement: switch body without case");
    }

    // Assemble the patterns
    if (get_op(statement) == CASE_KW) {
      str = 0;
      while (get_op(statement) == CASE_KW) {
        // This is much more permissive than what a C compiler would allow,
        // but Shell allows matching on arbitrary expression in case
        // patterns so it's fine. If we wanted to do this right, we'd check
        // that the pattern is a numeric literal or an enum identifier.
        str = concatenate_strings_with(str, comp_rvalue(get_child(statement, 0), RVALUE_CTX_BASE), wrap_char('|'));
        statement = get_child(statement, 1);
      }
    } else {
      str = wrap_str("*");
      statement = get_child(statement, 0);
    }

    append_glo_decl(string_concat(str, wrap_str(")")));

    nest_level += 1;

    in_tail_position = false;

    // case and default nodes contain the first statement of the block. We add it to the list of statements to process.
    node = new_ast2('{', statement, node); // Allocating memory isn't ideal but it makes the code tidier

    while (get_op(node) == '{') {
      statement = get_child(node, 0);
      node = get_child(node, 1);
      // If we encounter a break or return statement, we stop processing the block
      if (comp_switch_block_statement(statement, start_in_tail_position)) break;
    }

    nest_level -= 1;
    append_glo_decl(wrap_str(";;"));
  }

  nest_level -= 1;
  append_glo_decl(wrap_str("esac"));
}

void comp_statement(ast node, int else_if) {
  int op = get_op(node);
  text str;
  int start_loop_end_actions_start;
  int start_loop_end_actions_end;

  gensym_ix = 0;

  if (op == IF_KW) {
    append_glo_decl(string_concat3(
          wrap_str(else_if ? "elif " : "if "),
          comp_rvalue(get_child(node, 0), else_if ? RVALUE_CTX_TEST_ELSEIF : RVALUE_CTX_TEST),
          wrap_str(" ; then")
        ));

    nest_level += 1;
    if (get_child(node, 1) != 0) { comp_statement(get_child(node, 1), false); }
    else { append_glo_decl(wrap_char(':')); }
    nest_level -= 1;

    if (get_child(node, 2) != 0) {
      /* Compile sequence of if else if using elif */
      if (get_op(get_child(node, 2)) == IF_KW) {
        comp_statement(get_child(node, 2), true); /* comp_statement with else_if == true emits elif*/
      } else {
        append_glo_decl(wrap_str("else"));
        nest_level += 1;
        comp_statement(get_child(node, 2), false);
        nest_level -= 1;
      }
    }
    if (!else_if) append_glo_decl(wrap_str("fi"));
  } else if (op == WHILE_KW) {
    append_glo_decl(string_concat3(
      wrap_str("while "),
      comp_rvalue(get_child(node, 0), RVALUE_CTX_TEST),
      wrap_str(" ; do")
    ));

    loop_nesting_level += 1;
    nest_level += 1;
    if (get_child(node, 1) != 0) { comp_statement(get_child(node, 1), false); }
    else { append_glo_decl(wrap_char(':')); }
    nest_level -= 1;
    loop_nesting_level -= 1;

    append_glo_decl(wrap_str("done"));
  } else if (op == FOR_KW) {
    /* Save loop end actions from possible outer loop */
    start_loop_end_actions_start = loop_end_actions_start;
    start_loop_end_actions_end = loop_end_actions_end;

    if (get_child(node, 0)) comp_statement(get_child(node, 0), false);

    str = wrap_char(':'); /* Empty statement */
    if (get_child(node, 1)) {
      str = comp_rvalue(get_child(node, 1), RVALUE_CTX_TEST);
    }

    append_glo_decl(string_concat3(wrap_str("while "), str, wrap_str(" ; do")));

    /*
      This is a little bit of a hack, but it makes things so much simpler.
      Because we need to capture the code for the end of loop actions
      (increment, etc.), and those can be any statement expression, we somehow
      need to get the text generated by the call to comp_statement. Instead of
      modifying comp_statement to accomodate this, we just remove the code
      generated by comp_statement using undo_glo_decls and save the indices of
      the declarations of the loop end actions so they can replayed later.
    */
    loop_end_actions_start = glo_decl_ix;
    if (get_child(node, 2)) comp_statement(get_child(node, 2), false);
    undo_glo_decls(loop_end_actions_start);
    loop_end_actions_end = glo_decl_ix;

    loop_nesting_level += 1;
    nest_level += 1;
    if (get_child(node, 3) != 0) { comp_statement(get_child(node, 3), false); }
    else if (get_child(node, 2) == 0) { append_glo_decl(wrap_char(':')); }
    replay_glo_decls(loop_end_actions_start, loop_end_actions_end, true);

    nest_level -= 1;
    loop_nesting_level -= 1;
    loop_end_actions_start = start_loop_end_actions_start;
    loop_end_actions_end = start_loop_end_actions_end;

    append_glo_decl(wrap_str("done"));
  } else if (op == SWITCH_KW) {
    comp_switch(node);
  } else if (op == BREAK_KW) {
    if (loop_nesting_level == 0) fatal_error("comp_statement: break not in loop");
    /* TODO: What's the semantic of break? Should we run the end of loop action before breaking? */
    append_glo_decl(wrap_str("break"));
  } else if (op == CONTINUE_KW) {
    if (loop_nesting_level == 0) fatal_error("comp_statement: continue not in loop");
    replay_glo_decls(loop_end_actions_start, loop_end_actions_end, true);
    /* We could remove the continue when in tail position, but it's not worth doing */
    append_glo_decl(wrap_str("continue"));
  } else if (op == RETURN_KW) {
    if (get_child(node, 0) != 0) {
      if (get_op(get_child(node, 0)) == '(') { /* Check if function call */
        comp_fun_call(get_child(node, 0), new_ast0(IDENTIFIER_DOLLAR, 1));
      } else {
        append_glo_decl(string_concat3(
          wrap_str(": $(($1 = "),
          comp_rvalue(get_child(node, 0), RVALUE_CTX_ARITH_EXPANSION),
          wrap_str("))")
        ));
      }
    }
    if (in_tail_position AND loop_nesting_level == 1) {
      append_glo_decl(wrap_str("break")); /* Break out of the loop, and the function prologue will do the rest */
    } else if (in_tail_position && in_block_head_position && get_child(node, 0) == 0) {
      append_glo_decl(wrap_str(":")); /* Block only contains a return statement so it's not empty */
    } else if (!in_tail_position OR loop_nesting_level != 0) {
      rest_loc_var_fixups = new_ast2(',', append_glo_decl_fixup(), rest_loc_var_fixups);
      append_glo_decl(wrap_str("return"));
    }
  } else if (op == '(') { /* six.call */
    comp_fun_call(node, new_ast0(IDENTIFIER_EMPTY, 0)); /* Reuse IDENTIFIER_EMPTY ast? */
  } else if (op == '{') { /* six.compound */
    comp_body(node);
  } else if (op == '=') { /* six.x=y */
    comp_assignment(get_child(node, 0), get_child(node, 1));
  } else if (op == ':') {
    // Labelled statement are not very useful as gotos are not supported in the
    // Shell backend, but we still emit a label comment for readability.
    append_glo_decl(string_concat3(wrap_str("# "), wrap_str_pool(get_val(get_val(get_child(node, 0)))), wrap_char(':')));
    comp_statement(get_child(node, 1), false);
  } else if (op == GOTO_KW) {
    fatal_error("goto statements not supported");
  } else if (op == VAR_DECLS) {
    fatal_error("variable declaration must be at the beginning of a function");
  } else {
    str = comp_rvalue(node, RVALUE_CTX_BASE);
    if (contains_side_effects) {
      append_glo_decl(string_concat(wrap_str(": "), str));
    } else if (in_block_head_position && in_tail_position) {
      append_glo_decl(wrap_str(":")); /* Block only contains this statement so we have to make sure it's not empty */
    }
  }
}

ast get_leading_var_declarations(ast node) {
  ast result = 0;
  ast local_var;
  ast tail;
  ast new_tail;

  if (get_op(node) == '{') {
    while (get_op(node) == '{') {
      local_var = get_child(node, 0);
      if (get_op(local_var) != VAR_DECLS) break;

      /* Initialize list */
      new_tail = new_ast2(',', local_var, 0);
      if (result == 0) { result = new_tail; }
      else { set_child(tail, 1, new_tail); }
      tail = new_tail;

      node = get_child(node, 1);
    }
  }

  return new_ast2(',', result, node);
}

#ifdef OPTIMIZE_CONSTANT_PARAM
void mark_mutable_variables_statement(ast node) {
  int op = get_op(node);
  ast params;

  if (op == IF_KW) {
    mark_mutable_variables_statement(get_child(node, 0));
    if (get_child(node, 1)) mark_mutable_variables_body(get_child(node, 1));
    if (get_child(node, 2)) mark_mutable_variables_statement(get_child(node, 2));
  } else if (op == WHILE_KW) {
    mark_mutable_variables_statement(get_child(node, 0));
    if (get_child(node, 1)) mark_mutable_variables_body(get_child(node, 1));
  } else if (op == FOR_KW) {
    if (get_child(node, 0)) mark_mutable_variables_statement(get_child(node, 0));
    if (get_child(node, 1)) mark_mutable_variables_statement(get_child(node, 1));
    if (get_child(node, 2)) mark_mutable_variables_statement(get_child(node, 2));
    if (get_child(node, 3)) mark_mutable_variables_body(get_child(node, 2));
  } else if (op == SWITCH_KW) {
    mark_mutable_variables_statement(get_child(node, 0));
    if (get_child(node, 1)) mark_mutable_variables_statement(get_child(node, 1));
  } else if (op == BREAK_KW OR op == CONTINUE_KW OR op == GOTO_KW) {
    /* Do nothing */
  } else if (op == ':' || op == CASE_KW || op == DEFAULT_KW) {
    mark_mutable_variables_statement(get_child(node, op == DEFAULT_KW ? 0 : 1));
  } else if (op == RETURN_KW) {
    if (get_child(node, 0) != 0) mark_mutable_variables_statement(get_child(node, 0));
  } else if (op == '(') {
    params = get_child(node, 1);

    if (params != 0) { /* Check if not an empty list */
      if (get_op(params) == ',') {
        while (get_op(params) == ',') {
          mark_mutable_variables_statement(get_child(params, 0));
          params = get_child(params, 1);
        }
      } else { /* params is the first argument, not wrapped in a cons cell */
        mark_mutable_variables_statement(params);
      }
    }
  } else if (op == '{') { /* six.compound */
    mark_mutable_variables_body(node);
  } else if (op == IDENTIFIER OR op == IDENTIFIER_INTERNAL OR op == IDENTIFIER_STRING OR op == IDENTIFIER_DOLLAR OR op == INTEGER OR op == CHARACTER OR op == STRING) {
    /* Do nothing */
  } else if (op == '=' OR op == PLUS_PLUS_PRE OR op == MINUS_MINUS_PRE OR op == PLUS_PLUS_POST OR op == MINUS_MINUS_POST
         OR op == PLUS_EQ OR op == AMP_EQ OR op == BAR_EQ OR op == CARET_EQ OR op == LSHIFT_EQ OR op == MINUS_EQ
         OR op == PERCENT_EQ OR op == PLUS_EQ OR op == RSHIFT_EQ OR op == SLASH_EQ OR op == STAR_EQ OR op == SIZEOF_KW) {
    mark_variable_as_mutable(get_child(node, 0));
    if (get_nb_children(node) == 2) mark_mutable_variables_statement(get_child(node, 1));
  } else if ((op == '~') OR (op == '!')
      OR (op == '&') OR (op == '|') OR (op == '<') OR (op == '>') OR (op == '+') OR (op == '-') OR (op == '*') OR (op == '/')
      OR (op == '%') OR (op == '^') OR (op == ',') OR (op == EQ_EQ) OR (op == EXCL_EQ) OR (op == LT_EQ) OR (op == GT_EQ)
      OR (op == LSHIFT) OR (op == RSHIFT) OR (op == '=') OR (op == '[') OR (op == AMP_AMP) OR (op == BAR_BAR) OR (op == '.') OR (op == ARROW) OR (op == CAST)) {
    mark_mutable_variables_statement(get_child(node, 0));
    if (get_nb_children(node) == 2) mark_mutable_variables_statement(get_child(node, 1));
  } else if (op == '?') {
    mark_mutable_variables_statement(get_child(node, 0));
    mark_mutable_variables_statement(get_child(node, 1));
    mark_mutable_variables_statement(get_child(node, 2));
  } else {
    printf("op=%d %c\n", op, op);
    fatal_error("mark_mutable_variables_statement: unknown statement");
  }
}

void mark_mutable_variables_body(ast node) {
  if (node != 0) {
    while (get_op(node) == '{') {
      mark_mutable_variables_statement(get_child(node, 0));
      node = get_child(node, 1);
    }
  }
}
#endif

void comp_glo_fun_decl(ast node) {
  ast name = get_child(node, 0);
  ast fun_type = get_child(node, 1);
  ast params = get_child(node, 2);
  ast body = get_child(node, 3);
  ast local_vars_and_body, local_vars;
  text trailing_txt = 0;
  int params_ix;
  ast decls, vars, var;
  int save_loc_vars_fixup;

  if (body == -1) return; // ignore forward declarations

  local_vars_and_body = get_leading_var_declarations(get_child(node, 3));
  local_vars = get_child(local_vars_and_body, 0);
  body = get_child(local_vars_and_body, 1);

  top_level_stmt = false;

  assert_vars_are_safe(params, true);
  assert_vars_are_safe(local_vars, true);

  // If the function is main
  if (name == MAIN_ID) {
    main_defined = true;
    // If main has parameters. If so, we'll prepare the argc/argv values in the prologue.
    if (params != 0) runtime_use_make_argv = true;
    // Check if main returns an exit code.
    if (get_op(fun_type) != VOID_KW) main_returns = true;
  }

  add_fun_params_to_local_env(params, 2, KIND_PARAM); /* Start position at 2 because 1 is taken by result_loc */
  add_vars_to_local_env(local_vars, local_env_size + 2, KIND_LOCAL);

#ifdef OPTIMIZE_CONSTANT_PARAM
  mark_mutable_variables_body(body);
#endif

#ifdef SH_INITIALIZE_PARAMS_WITH_LET
  trailing_txt = let_params(params);
  if (trailing_txt != 0) trailing_txt = string_concat(wrap_char(' '), trailing_txt);
#endif

  if (trailing_txt == 0) {
    /* Show the mapping between the function parameters and $1, $2, etc. */
    params_ix = 2; /* Start at 2 because $1 is assigned to result location */
    while (params != 0) {
      var = get_child(params, 0);
      trailing_txt = concatenate_strings_with(trailing_txt, string_concat3(wrap_str_pool(get_val(get_val(var))), wrap_str(": $"), wrap_int(params_ix)), wrap_str(", "));
      params = get_child(params, 1);
      params_ix += 1;
    }
    if (trailing_txt != 0) trailing_txt = string_concat(wrap_str(" # "), trailing_txt);
  }

  append_glo_decl(string_concat3(
    function_name(name),
    wrap_str("() {"),
    trailing_txt
  ));

  in_tail_position = true;
  nest_level += 1;

  save_loc_vars_fixup = append_glo_decl_fixup(); /* Fixup is done after compiling body */

#ifndef SH_INITIALIZE_PARAMS_WITH_LET
  /* Initialize parameters */
  params = get_child(node, 2); /* Reload params because params is now = 0 */
  params_ix = 2;
  while (params != 0) {
    var = get_child(params, 0);

    /* Constant parameters don't need to be initialized */
    if (!variable_is_constant_param(find_var_in_local_env(get_val(var)))) {
      comp_assignment(new_ast0(IDENTIFIER, get_child(var, 0)), new_ast0(IDENTIFIER_DOLLAR, params_ix));
    }

    params = get_child(params, 1);
    params_ix += 1;
  }
#endif

  /* Initialize local vars */
  while (local_vars != 0) {
    decls = get_child(local_vars, 0); /* List of VAR_DECLS */
    vars = get_child(decls, 0); /* VAR_DECL list */
    while(vars != 0) {
      var = get_child(vars, 0); /* Single VAR_DECL */
      /* TODO: Replace with ternary expression? */
      if (get_child(var, 2) != 0) {
        comp_assignment(new_ast0(IDENTIFIER, get_child(var, 0)), get_child(var, 2));
      }
#ifdef INITIALIZE_LOCAL_VARS_WITH_ZERO
      else {
        comp_assignment(new_ast0(IDENTIFIER, get_child(var, 0)), new_ast0(INTEGER, 0));
      }
#endif
      vars = get_child(vars, 1); /* Next VAR_DECL */
    }
    local_vars = get_child(local_vars, 1);
  }

  if (body == 0) {
    append_glo_decl(wrap_str(":")); // Empty function
  } else {
    comp_body(body);
  }

  append_glo_decl(restore_local_vars(params_ix - 1));

  /*
    We only know the full set of temporary variables after compiling the function body.
    So we fixup the calls to save_vars and unsave_vars at the end.
  */
  fixup_glo_decl(save_loc_vars_fixup, save_local_vars(params_ix - 1));
  while (rest_loc_var_fixups != 0) {
    fixup_glo_decl(get_child(rest_loc_var_fixups, 0), restore_local_vars(params_ix - 1));
    rest_loc_var_fixups = get_child(rest_loc_var_fixups, 1);
  }

  nest_level -= 1;

  append_glo_decl(wrap_str("}\n"));
}

text comp_constant(ast node) {
  int op = get_op(node);
  ast new_ident;

  if (op == INTEGER) {
    return wrap_int(-get_val(node));
  } else if (op == CHARACTER) {
    return string_concat(wrap_char('$'), character_ident(get_val(node)));
  } else if (op == STRING) {
    runtime_use_defstr = true;
    new_ident = fresh_string_ident();
    comp_defstr(new_ident, get_val(node));
    return format_special_var(new_ident, false);
  } else if ((op == '-') AND get_nb_children(node) == 1) {
    return string_concat(wrap_char('-'), comp_constant(get_child(node, 0)));
  } else {
    fatal_error("comp_constant: unknown constant");
    return 0;
  }
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
  if ((get_op(type) == '[' && get_op(get_child(type, 1)) == STRUCT_KW && get_val(get_child(type, 1)) == 0)
    || (get_op(type) == STRUCT_KW AND get_val(type) == 0)) {
    printf("%s ", string_pool + get_val(name));
    fatal_error("array of struct and struct value type are not supported in shell backend. Use a reference type instead.");
  }

  if (get_op(type) == '[') { /* Array declaration */
    runtime_defarr();
    append_glo_decl(
      string_concat4(
        wrap_str("defarr "),
        env_var(new_ast0(IDENTIFIER, name)),
        wrap_char(' '),
        wrap_int(get_val(get_child(type, 0)))
      )
    );
  } else {
#ifdef SUPPORT_ADDRESS_OF_OP
    runtime_defglo();
    append_glo_decl(
      string_concat4(
        wrap_str("defglo "),
        env_var(new_ast0(IDENTIFIER, name)),
        wrap_char(' '),
        comp_constant(init)
      )
    );
#else
    comp_assignment(new_ast0(IDENTIFIER, name), init);
#endif
  }
}

void comp_assignment_constant(text constant_name, ast rhs) {
  append_glo_decl(string_concat4(wrap_str("readonly "), constant_name, wrap_char('='), comp_rvalue(rhs, RVALUE_CTX_BASE)));
}

// Enums are just like global variables, but they are readonly.
// Since anything that's not a local variable is considered global, this makes
// it easy to implement enums.
void comp_enum_cases(ast ident, ast cases) {
  if (ident != 0) {
    append_glo_decl(string_concat3(wrap_str("# "), wrap_str_pool(get_val(get_val(ident))), wrap_str(" enum declaration")));
  } else {
    append_glo_decl(wrap_str("# Enum declaration"));
  }
  while (get_op(cases) == ',') {
    comp_assignment_constant(env_var(get_child(cases, 0)), get_child(cases, 1));
    cases = get_child(cases, 2);
  }
}

/*
  Struct member access is implemented like array indexing. Each member is mapped
  to a readonly variable containing the offset of the member and accessing to
  s->a is equivalent to *(s + a).

  For example, for the struct:

    struct Point {
      int x;
      int y;
    }

    Point *p = malloc(sizeof(Point));
    p->y = 42;

  The following code is generated:

    readonly __x=0
    readonly __y=1
    readonly __sizeof__Point=2

    _malloc p $((__sizeof__Point))
    : $(( _$((p + __x)) = 42 ))

  This approach doesn't work when the same member name is used in different
  structs, but it makes for readable code and is simple to implement.
  Because the member offset variables are declared as readonly, name conflicts
  will result in a runtime error when the shell program initializes.
*/
void comp_struct(ast ident, ast members) {
  int offset = new_ast0(INTEGER, 0);
  int field_type;
  if (ident != 0) {
    append_glo_decl(string_concat3(wrap_str("# "), wrap_str_pool(get_val(get_val(ident))), wrap_str(" struct member declarations")));
  } else {
    append_glo_decl(wrap_str("# Struct member declarations"));
  }
  while (get_op(members) == ',') {
    field_type = get_child(members, 1);
    comp_assignment_constant(struct_member_var(get_child(members, 0)), offset);
    members = get_child(members, 2);

    // Arrays and struct value types are not supported for now.
    // When we have type information on the local and global variables, we'll
    // be able to generate the correct code for these cases.
    if (get_op(field_type) == '[' || (get_op(field_type) == STRUCT_KW && get_val(field_type) == 0)) {
      fatal_error("Nested structures not supported by shell backend. Use a reference type instead.");
    } else {
      set_val(offset, get_val(offset) - 1);
    }
  }

  if (ident != 0) {
    comp_assignment_constant(struct_sizeof_var(ident), offset);
  }

  append_glo_decl(0); // newline
}

void handle_enum_struct_union_type_decl(ast type) {
  if (get_op(type) == ENUM_KW) {
    comp_enum_cases(get_child(type, 1), get_child(type, 2));
  } else if (get_op(type) == STRUCT_KW) {
    comp_struct(get_child(type, 1), get_child(type, 2));
  } else if (get_op(type) == UNION_KW) {
    fatal_error("handle_enum_struct_union_type_decl: union not supported");
  }

  // If not an enum, struct, or union, do nothing
}

/*
  This function compiles 1 top level declaration at the time.
  The supported top level declarations are:
    - global variable declarations
    - global variable assignments
    - function declarations
    - enum declarations
    - struct declarations
*/
void comp_glo_decl(ast node) {
  ast declarations;
  ast variable;
  int op = get_op(node);
  fun_gensym_ix = 0;

  top_level_stmt = true;

  if (op == '=') { /* Assignments */
   comp_assignment(get_child(node, 0), get_child(node, 1));
  } else if (op == VAR_DECLS) { /* Variable declarations */
    declarations = get_child(node, 0);
    while (declarations != 0) { /* Multiple variable declarations */
      variable = get_child(declarations, 0); /* Single variable declaration */
      comp_glo_var_decl(variable); /* Compile variable declaration */
      declarations = get_child(declarations, 1); /* Next variable declaration */
    }
  } else if (op == FUN_DECL) {
    comp_glo_fun_decl(node);
  } else if (op == TYPEDEF_KW) {
    handle_enum_struct_union_type_decl(get_child(node, 1));
  } else if (op == ENUM_KW OR op == STRUCT_KW OR op == UNION_KW) {
    handle_enum_struct_union_type_decl(node);
  } else {
    printf("op=%d %c with %d children\n", op, op, get_nb_children(node));
    fatal_error("comp_glo_decl: unexpected declaration");
  }
}

void prologue() {
  putstr("#!/bin/sh\n");
  putstr("set -e -u\n\n");
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
      main_args = wrap_str(" $__argc_for_main $__argv_for_main");
    }

    if (main_returns) {
      putstr("__code=0; # Success exit code\n");
      print_text(string_concat3(wrap_str("_main __code"), main_args, wrap_str("; exit $__code\n")));
    } else {
      print_text(string_concat3(wrap_str("_main __"), main_args, wrap_char('\n')));
    }
  }
}

/* Initialize local and synthetic variables used by function */
void initialize_function_variables() {
  ast env = local_env;
  ast local_var;
  ast ident;
  text res = 0;
  int counter = fun_gensym_ix;

  while (counter > 0) {
    ident = new_ast0(IDENTIFIER_INTERNAL, wrap_int(counter));
    res = concatenate_strings_with(res, format_special_var(ident, false), wrap_str(" = "));
    counter -= 1;
  }

  while (env != 0) {
    local_var = get_child(env, 0);
    ident = new_ast0(IDENTIFIER, get_child(local_var, 0));

    if (!variable_is_constant_param(local_var)) {
      res = concatenate_strings_with(res, env_var(ident), wrap_str(" = "));
    }

    env = get_child(env, 1);
  }

  if (res != 0) {
    res = string_concat3(wrap_str(": $(("), res, wrap_str(" = 0))"));
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
  /* Reset state */
  glo_decl_ix = 0;
  local_env_size = 0;
  local_env = 0;
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
