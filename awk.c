// AWK codegen

// Memory stats

#ifdef PRINT_MEMORY_STATS
int max_text_alloc = 0;
int cumul_text_alloc = 0;
#endif

// codegen

// Rope-like text representation
#include "text.c"

// Environment tracking
#include "env.c"

// Environment tracking
#include "glo_decls.c"

// Codegen context
bool main_defined = false;          // If the main function is defined
bool runtime_use_make_argv = false;
bool init_block_open = false; // If we're inside an initialization block
int  init_block_id = 0; // Identifier of the current initialization block

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

#define comp_rvalue(node) comp_rvalue_go((node), 0)
text comp_rvalue_go(ast node, int outer_op);
text comp_fun_call(ast node, ast params);
bool comp_body(ast node, STMT_CTX stmt_ctx);
bool comp_statement(ast node, STMT_CTX stmt_ctx);
void mark_mutable_variables_body(ast node);
void handle_enum_struct_union_type_decl(ast node);
ast handle_side_effects_go(ast node, bool executes_conditionally);

// AWK-specific output functions
void print_awk_shebang() {
  putstr("#!/usr/bin/awk -f\n");
}

void print_awk_comment(char *comment) {
  putchar('#');
  putchar(' ');
  print_text(wrap_str_lit(comment));
  putchar('\n');
}

void add_var_to_local_env(ast decl, enum BINDING kind) {
  int ident_symbol = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, decl, 0));

  // Make sure we're not shadowing an existing local variable
  if (cgc_lookup_var(ident_symbol, cgc_locals)) {
    dump_ident(ident_symbol);
    fatal_error("Local variable shadowing is not supported.");
  }

  // The var is not part of the environment, so we add it.
  cgc_add_local_var(kind, ident_symbol, get_child_(DECL, decl, 1));
}

text global_var(int ident_symbol) {
  return string_concat(wrap_char('_'), wrap_str_pool(ident_symbol));
}

text local_var(int ident_symbol) {
  return wrap_str_pool(ident_symbol);
}

text env_var(ast ident) {
  int binding;
  int ident_symbol = get_val_(IDENTIFIER, ident);
  if ((binding = cgc_lookup_var(ident_symbol, cgc_locals))) {
    return local_var(ident_symbol);
  } else {
    return global_var(ident_symbol);
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

// NOTE: Same as sh.c, except with _$(( )) replaced by _[  ]
text comp_lvalue(ast node) {
  int op = get_op(node);
  text sub1;
  text sub2;

  if (op == IDENTIFIER) {
    return env_var(node);
  } else if (op == '[') {
    sub1 = comp_rvalue(get_child_('[', node, 0));
    sub2 = comp_rvalue(get_child_('[', node, 1));
    return string_concat5(wrap_str_lit("_["), sub1, wrap_str_lit(" + "), sub2, wrap_str_lit("]"));
  } else if (op == '*') {
    sub1 = comp_rvalue(get_child_('*', node, 0));
    return string_concat(wrap_char('_'), sub1);
  }
#ifdef SUPPORT_STRUCT_UNION
  else if (op == ARROW) {
    sub1 = comp_rvalue(get_child_(ARROW, node, 0));
    sub2 = struct_member_var(get_child_(ARROW, node, 1));
    return string_concat5(wrap_str_lit("_["), sub1, wrap_str_lit(" + "), sub2, wrap_str_lit("]"));
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

text op_to_str(int op) {
  if      (32 < op && op < 127) return string_concat3(wrap_char(' '), wrap_char(op), wrap_char(' '));
  else if (op == AMP_AMP)    return wrap_str_lit(" && ");
  else if (op == AMP_EQ)     return wrap_str_lit(" &= ");
  else if (op == BAR_BAR)    return wrap_str_lit(" || ");
  else if (op == BAR_EQ)     return wrap_str_lit(" |= ");
  else if (op == CARET_EQ)   return wrap_str_lit(" ^= ");
  else if (op == EQ_EQ)      return wrap_str_lit(" == ");
  else if (op == GT_EQ)      return wrap_str_lit(" >= ");
  else if (op == LSHIFT_EQ)  return wrap_str_lit(" <<= ");
  else if (op == LT_EQ)      return wrap_str_lit(" <= ");
  else if (op == MINUS_EQ)   return wrap_str_lit(" -= ");
  else if (op == EXCL_EQ)    return wrap_str_lit(" != ");
  else if (op == PERCENT_EQ) return wrap_str_lit(" %= ");
  else if (op == PLUS_EQ)    return wrap_str_lit(" += ");
  else if (op == RSHIFT_EQ)  return wrap_str_lit(" >>= ");
  else if (op == SLASH_EQ)   return wrap_str_lit(" /= ");
  else if (op == STAR_EQ)    return wrap_str_lit(" *= ");
  else {
    dump_op(op);
    fatal_error("op_to_str: unexpected operator");
    return 0;
  }
}

// '&' || op == '|' || op == '^' || op == LSHIFT || op == RSHIFT
text bitwise_op_to_str(int op) {
       if (op == '&' || op == AMP_EQ)       return wrap_str_lit("and");
  else if (op == '|' || op == BAR_EQ)       return wrap_str_lit("or");
  else if (op == '^' || op == CARET_EQ)     return wrap_str_lit("xor");
  else if (op == '~')                       return wrap_str_lit("compl");
  else if (op == LSHIFT || op == LSHIFT_EQ) return wrap_str_lit("lshift");
  else if (op == RSHIFT || op == RSHIFT_EQ) return wrap_str_lit("rshift");
  else {
    dump_op(op);
    fatal_error("bitwise_op_to_str: unexpected operator");
    return 0;
  }
}

// Return true if the operator is associative.
// Associative operators can be chained without parentheses.
bool is_associative_operator(int op) {
  return (op == '+')   | (op == '*')     | (op == '&')    | (op == '|')    | (op == '^')
      |  (op == EQ_EQ) | (op == AMP_AMP) | (op == BAR_BAR);
}

text wrap_if_needed(text code, int outer_op, int inner_op) {
  // Rough heuristic to determine if we need to wrap in parentheses. If we
  // wanted to do this right, we'd track the left and right operators and
  // use this information to determine if parentheses are needed.
  if ( outer_op != 0
    && outer_op != '=' // Assignment has the lowest precedence so we never use parentheses
    && (!is_associative_operator(inner_op) || inner_op != outer_op) // Adjacent associative operations don't need parentheses
    ) {
    return string_concat3(wrap_char('('), code, wrap_char(')'));
  } else {
    return code;
  }
}

text comp_assignment(ast lvalue, ast rvalue) {
  text code_lvalue = comp_lvalue(lvalue);
  text code_rvalue = comp_rvalue(rvalue);
  return string_concat3(code_lvalue, wrap_str_lit(" = "), code_rvalue);
}

text comp_rvalue_go(ast node, int outer_op) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  text sub1, sub2, sub3;
  ast child0, child1, child2;

  if (nb_children >= 1) { child0 = get_child(node, 0); }
  if (nb_children >= 2) { child1 = get_child(node, 1); }
  if (nb_children >= 3) { child2 = get_child(node, 2); }

  if (nb_children == 0) {
    if (op == INTEGER
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
      || op == INTEGER_HEX || op == INTEGER_OCT
#endif
      ) {
      return wrap_integer(1, node);
    }
    else if (op == CHARACTER) {
      // For characters, return ord['c']:
      // FIXME: This likely only works for printable characters.
      return string_concat3(wrap_str_lit("ord["), wrap_char(get_val_(CHARACTER, node)), wrap_str_lit("]"));
    } else if (op == STRING) {
      // For string, call defstr("...") to define the string and return its identifier
      return string_concat3(wrap_str_lit("defstr(\""), escape_text(wrap_str_pool(get_val_(STRING, node)), false), wrap_str_lit("\")"));
    } else if (op == IDENTIFIER) {
      return env_var(node);
    } else {
      dump_node(node);
      fatal_error("comp_rvalue_go: unexpected operator");
      return 0;
    }
  } else if (nb_children == 1) {
    if (op == '*') {
      sub1 = comp_rvalue_go(child0, op);
      return string_concat3(wrap_str_lit("_["), sub1, wrap_str_lit("]"));
    } else if (op == '+') {
      // +x is equivalent to x
      return comp_rvalue_go(child0, outer_op);
    } else if (op == '-' || op == '!') {
      if (op == '-' && (get_op(child0) == INTEGER
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
       || op == INTEGER_HEX || op == INTEGER_OCT
#endif
      )
      ) {
        return wrap_integer(-1, child0);
      } else {
        sub1 = comp_rvalue_go(child0, op);
        return string_concat(wrap_char(op), sub1);
      }
    } else if (op == '~') {
      // Complement operator is a function in AWK
      sub1 = comp_rvalue_go(child0, op);
      return string_concat3(wrap_str_lit("compl("), sub1, wrap_char(')'));
    } else if (op == MINUS_MINUS_PRE) {
      sub1 = comp_lvalue(child0);
      return string_concat(sub1, wrap_str_lit("--"));
    } else if (op == PLUS_PLUS_PRE) {
      sub1 = comp_lvalue(child0);
      return string_concat(sub1, wrap_str_lit("++"));
    } else if (op == MINUS_MINUS_POST) {
      sub1 = comp_lvalue(child0);
      return string_concat(wrap_str_lit("--"), sub1);
    } else if (op == PLUS_PLUS_POST) {
      sub1 = comp_lvalue(child0);
      return string_concat(wrap_str_lit("++"), sub1);
    }
#ifdef SUPPORT_SIZEOF
    else if (op == SIZEOF_KW) {
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
            return wrap_int(1);

#ifdef SUPPORT_STRUCT_UNION
          case STRUCT_KW:
            return struct_sizeof_var(get_child__(STRUCT_KW, IDENTIFIER, child0, 1));
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
    }
#endif // SUPPORT_SIZEOF
#ifdef SH_SUPPORT_ADDRESS_OF
    else if (op == '&') {
      return comp_lvalue_address(child0);
    }
#endif
    else {
      dump_node(node);
      fatal_error("comp_rvalue_go: unexpected operator");
      return 0;
    }
  } else if (nb_children == 2) {
    if (op == '+' || op == '-' || op == '*' || op == '/' || op == '%') { // TODO: op == ','
      sub1 = comp_rvalue_go(child0, op);
      sub2 = comp_rvalue_go(child1, op);
      return wrap_if_needed(string_concat3(sub1, op_to_str(op), sub2), outer_op, op);
    } else if (op == '&' || op == '|' || op == '^' || op == LSHIFT || op == RSHIFT) {
      // These operators are functions in AWK
      sub1 = comp_rvalue_go(child0, op);
      sub2 = comp_rvalue_go(child1, op);
      return wrap_if_needed(
              string_concat(
                bitwise_op_to_str(op),
                string_concat5(wrap_char('('), sub1, wrap_str_lit(", "), sub2, wrap_char(')'))), outer_op, op);
    } else if (op == '=' || op == MINUS_EQ || op == PERCENT_EQ || op == PLUS_EQ || op == SLASH_EQ || op == STAR_EQ) {
      sub1 = comp_lvalue(child0);
      sub2 = comp_rvalue_go(child1, op);
      return wrap_if_needed(string_concat3(sub1, op_to_str(op), sub2), outer_op, op);
    } else if (op == AMP_EQ || op == BAR_EQ || op == CARET_EQ || op == LSHIFT_EQ || op == RSHIFT_EQ) {
      sub1 = comp_lvalue(child0);
      sub2 = comp_rvalue_go(child1, op);
      return wrap_if_needed(
              string_concat(
                string_concat3(sub1, wrap_str_lit(" = "), bitwise_op_to_str(op)),
                string_concat5(wrap_char('('), sub1, wrap_str_lit(", "), sub2, wrap_char(')'))),
              outer_op, '=');
    } else if (op == '[') { // array indexing
      sub1 = comp_rvalue_go(child0, '+');
      sub2 = comp_rvalue_go(child1, '+');
      return string_concat5(wrap_str_lit("_["), sub1, wrap_str_lit(" + "), sub2, wrap_str_lit("]"));
    }
#ifdef SUPPORT_STRUCT_UNION
    else if (op == ARROW) { // member access is implemented like array access
      sub1 = comp_rvalue_go(child0, op);
      sub2 = struct_member_var(child1);
      return string_concat5(wrap_str_lit("_$(("), sub1, wrap_str_lit(" + "), sub2, wrap_str_lit("))"));
    }
#endif
    else if (op == EQ_EQ || op == EXCL_EQ || op == LT_EQ || op == GT_EQ || op == '<' || op == '>') {
      sub1 = comp_rvalue_go(child0, op);
      sub2 = comp_rvalue_go(child1, op);
      return wrap_if_needed(string_concat3(sub1, op_to_str(op), sub2), outer_op, op);
    } else if (op == CAST) { // Casts are no-op
      return comp_rvalue_go(child1, op);
    } else if (op == AMP_AMP || op == BAR_BAR) {
      sub1 = comp_rvalue_go(child0, op);
      sub2 = comp_rvalue_go(child1, op);
      return wrap_if_needed(string_concat3(sub1, op_to_str(op), sub2), outer_op, op);
    } else if (op == '(') {
      return comp_fun_call(child0, child1);
    } else {
      dump_node(node);
      fatal_error("comp_rvalue_go: unknown rvalue");
      return 0;
    }
  } else if (nb_children == 3) {
    if (op == '?') {
      sub1 = comp_rvalue_go(child0, op);
      sub2 = comp_rvalue_go(child1, op);
      sub3 = comp_rvalue_go(child2, op);
      return wrap_if_needed(string_concat5(sub1, op_to_str(op), sub2, wrap_str_lit(" : "), sub3), outer_op, op);
    } else {
      dump_node(node);
      fatal_error("comp_rvalue_go: unknown rvalue");
      return 0;
    }
  } else {
    dump_node(node);
    fatal_error("comp_rvalue_go: unknown rvalue");
    return 0;
  }
}

bool comp_body(ast node, STMT_CTX stmt_ctx) {
  int start_cgc_locals = cgc_locals;

  while (node != 0) {
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
text make_switch_pattern(ast statement, text scrutinee_text) {
  text str = 0;

  while (1) { // statement will never be null
    switch (get_op(statement)) {
      case DEFAULT_KW:
        str = wrap_int(1); // Default case always matches
        statement = get_child_(DEFAULT_KW, statement, 0);
        break;

      case CASE_KW:
        // This is much more permissive than what a C compiler would allow,
        // but Shell allows matching on arbitrary expression in case
        // patterns so it's fine. If we wanted to do this right, we'd check
        // that the pattern is a numeric literal or an enum identifier.
        str = concatenate_strings_with(str,
          string_concat3(comp_rvalue(get_child_(CASE_KW, statement, 0)), wrap_str_lit(" == "), scrutinee_text),
          wrap_str_lit(" || "));
        statement = get_child_(CASE_KW, statement, 1);
        break;

      default:
        if (str == 0) fatal_error("Expected case in switch. Fallthrough is not supported.");
        last_stmt = statement;
        return string_concat(str, wrap_char(')'));
    }
  }
}

// Warning: because pnut-awk doesn't support temporary variables, it uses a
// hardcoded global variable __scrutinee to hold the scrutinee value when
// needed. I _think_ this is fine for nested and reentrant switch statements
// since the scrutinee value is only needed to dispatch to the correct case,
// and can be overwritten in the body of the case statement (where nested switches
// and reentrancy would happen).
bool comp_switch(ast node) {
  ast statement;
  int start_cgc_locals = cgc_locals;
  bool first_case = true; // Whether we're compiling the first case/default statement or subsequent ones

  text scrutinee_text = comp_rvalue(get_child_(SWITCH_KW, node, 0));
  switch (get_op(get_child_(SWITCH_KW, node, 0))) {
    case IDENTIFIER:
    case INTEGER:
      // For "atomic" scrutinees, use them directly in the comparisons
      break;
    default:
      // Otherwise, evaluate the scrutinee into a temporary variable
      scrutinee_text = string_concat(wrap_str_lit("__scrutinee="), scrutinee_text);
  }

  cgc_add_enclosing_switch(false);

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

    append_glo_decl(string_concat3(
        wrap_str_lit(first_case ? "if (" : "} else if ("),
        make_switch_pattern(statement, scrutinee_text),
        wrap_str_lit(") {")
      ));
    first_case = false;
    statement = last_stmt; // last_stmt is set by make_switch_pattern

    nest_level += 1;

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
  append_glo_decl(wrap_str_lit("}")); // End of emulated case statement

  cgc_locals = start_cgc_locals;

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
          wrap_str_lit(else_if ? "else if (" : "if ("),
          comp_rvalue(get_child_(IF_KW, node, 0)),
          wrap_str_lit(")")
        ));

  nest_level += 1;
  start_glo_decl_idx = glo_decl_ix;
  termination_lhs = comp_statement(get_child_(IF_KW, node, 1), stmt_ctx);
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
  // We could remove the continue when in tail position, but it's not worth doing
  append_glo_decl(wrap_str_lit("continue"));
  return false;
}

bool comp_return(ast return_value) {
  if (return_value != 0) {
    append_glo_decl(string_concat(wrap_str_lit("return "), comp_rvalue(return_value)));
  } else {
    append_glo_decl(wrap_str_lit("return"));
  }
  return true;
}

// Since global and internal variables are prefixed with _, we restrict the name
// of variables to not start with _.
// Also, the AWK backend doesn't support variables with aggregate types.
void assert_var_decl_is_safe(ast variable, bool local) { // Helper function for assert_idents_are_safe
  ast ident_symbol = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, variable, 0));
  char* name = symbol_buf(ident_symbol);
  ast type = get_child_(DECL, variable, 1);
  if (name[0] == '_') { // Underscore is used to prefix global and internal variables
    dump_string("Variable name: ", name);
    fatal_error("variable name is invalid. It can't start with '_'.");
  }

  // FIXME: AWK has special variables that can't be used as regular variables.

  if (local) {
    // Local variables don't correspond to memory locations, and can't store more than 1 number/pointer.
    if (get_op(type) == '['
#ifdef SUPPORT_STRUCT_UNION
    || get_op(type) == STRUCT_KW
#endif
       ) {
      dump_string("Variable name: ", name);
      fatal_error("local array/struct value type is not supported for AWK backend. Use a reference type instead.");
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
      fatal_error("global array of struct and struct value type are not supported in AWK backend. Use a reference type instead.");
    }
  }
}

void comp_var_decls(ast node) {
  ast var_decl;

#ifdef SUPPORT_TYPE_SPECIFIERS
  switch (get_child_(DECLS, node, 1)) {
    // AUTO_KW and REGISTER_KW can simply be ignored.
    case EXTERN_KW:
    case STATIC_KW:
      fatal_error("Extern and static storage class specifier not supported on local variables");
      break;
  }
#endif
  node = get_child_opt_(DECLS, LIST, node, 0);
  while (node != 0) {
    // Add to local env and cummulative env, then initialize
    var_decl = car_(DECL, node);
    assert_var_decl_is_safe(var_decl, true);
    add_var_to_local_env(var_decl, BINDING_VAR_LOCAL);
    if (get_child_(DECL, var_decl, 2) != 0) { // Initializer
      append_glo_decl(comp_assignment(get_child__(DECL, IDENTIFIER, var_decl, 0), get_child_(DECL, var_decl, 2)));
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

  if (op == IF_KW) {
    return comp_if(node, stmt_ctx);
  } else if (op == WHILE_KW) {
    cgc_add_enclosing_loop();
    append_glo_decl(string_concat3(wrap_str_lit("while ("),
                                   comp_rvalue(get_child_(WHILE_KW, node, 0)),
                                   wrap_str_lit(") {")));
    nest_level += 1;
    comp_statement(get_child_(WHILE_KW, node, 1), stmt_ctx);
    nest_level -= 1;
    append_glo_decl(wrap_str_lit("}"));
    return false;
#ifdef SUPPORT_DO_WHILE
  } else if (op == DO_KW) {
    cgc_add_enclosing_loop();
    append_glo_decl(wrap_str_lit("do {"));
    nest_level += 1;
    comp_statement(get_child_(DO_KW, node, 0), stmt_ctx);
    nest_level -= 1;
    append_glo_decl(string_concat3(wrap_str_lit("} while ("),
                                   comp_rvalue(get_child_(DO_KW, node, 1)),
                                   wrap_str_lit(");")));
    return false;
#endif
  } else if (op == FOR_KW) {
    cgc_add_enclosing_loop();
    str = comp_rvalue(get_child_(FOR_KW, node, 0));
    str = string_concat(str, wrap_str_lit("; "));
    str = string_concat(str, comp_rvalue(get_child_(FOR_KW, node, 1)));
    str = string_concat(str, wrap_str_lit("; "));
    str = string_concat(str, comp_rvalue(get_child_(FOR_KW, node, 2)));
    append_glo_decl(string_concat3(wrap_str_lit("for ("),
                                   str,
                                   wrap_str_lit(") {")));
    nest_level += 1;
    comp_statement(get_child_(FOR_KW, node, 3), stmt_ctx);
    nest_level -= 1;
    append_glo_decl(wrap_str_lit("}"));
    return false;
  } else if (op == SWITCH_KW) {
    return comp_switch(node);
  } else if (op == BREAK_KW) {
    return comp_break(); // Break out of switch statement
  } else if (op == CONTINUE_KW) {
    return comp_continue(); // Continue to next iteration of loop
  } else if (op == RETURN_KW) {
    return comp_return(get_child_(RETURN_KW, node, 0));
  } else if (op == '(') { // Function call
    append_glo_decl(comp_fun_call(get_child_('(', node, 0), get_child_('(', node, 1)));
    return false;
  } else if (op == '{') { // Compound statement
    return comp_body(node, stmt_ctx);
#ifdef SUPPORT_GOTO
  } else if (op == ':') { // Labelled statement
    // Labelled statement are not very useful as gotos are not supported in the
    // AWK backend, but we still emit a label comment for readability.
    append_glo_decl(string_concat3(wrap_str_lit("#_ "), wrap_str_pool(get_val_(IDENTIFIER, get_child_(':', node, 0))), wrap_char(':')));
    return comp_statement(get_child_(':', node, 1), stmt_ctx);
  } else if (op == GOTO_KW) {
    fatal_error("goto statements not supported");
    return false;
#endif
  } else if (get_op(node) == CASE_KW || get_op(node) == DEFAULT_KW) {
    fatal_error("case/default must be at the beginning of a switch conditional block");
    return false;
  } else if (op == DECLS) {
    comp_var_decls(node);
    return false;
  } else {
    append_glo_decl(comp_rvalue(node));
    return false;
  }
}

text comp_local_variables() {
  // From cgc_locals, generate code to declare local variables as extra function
  // parameters.
  text params_text = 0;
  int env = cgc_locals_fun;
  int i;
  while (env != 0) {
    params_text = concatenate_strings_with( local_var(binding_ident(env))
                                          , params_text
                                          , wrap_str_lit(", "));
    env = binding_next(env);
  }

  return params_text;
}

void handle_function_params(ast lst) {
  while (lst != 0) {
    ast decl = car_(DECL, lst);
    assert_var_decl_is_safe(decl, true);
    add_var_to_local_env(decl, BINDING_PARAM_LOCAL);
    lst = tail(lst);
  }
}

void comp_glo_fun_decl(ast node) {
  ast fun_decl = get_child__(FUN_DECL, DECL, node, 0);
  ast body = get_child_opt_(FUN_DECL, '{', node, 1);
  ast name_symbol = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, fun_decl, 0));
  ast fun_type = get_child__(DECL, '(', fun_decl, 1);
  ast params = get_child_opt_('(', LIST, fun_type, 1);
  text local_vars_text = 0;
  int local_vars_decl_fixup;
  ast decl;

  if (body == -1) return; // ignore forward declarations

  handle_function_params(params);

  // If the function is main
  if (name_symbol == MAIN_ID) {
    main_defined = true;
    // If main has parameters, we'll prepare the argc/argv values in the epilogue.
    if (params != 0) runtime_use_make_argv = true;
  }

  local_vars_decl_fixup = append_glo_decl_fixup(); // Fixup is done after compiling body

  nest_level += 1;
  comp_body(body, STMT_CTX_DEFAULT);
  nest_level -= 1;
  append_glo_decl(wrap_str_lit("}\n"));

  // Fixup local variable declarations

  text fun_decl_text = string_concat5(
    wrap_str_lit("function "),
    function_name(name_symbol),
    wrap_str_lit("("),
    comp_local_variables(),
    wrap_str_lit(") {")
  );
  fixup_glo_decl(local_vars_decl_fixup, fun_decl_text);
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

    if (arr_len == 0) {
      fatal_error("Array declaration without size or initializer list");
    }

    append_glo_decl(
      string_concat4(
        global_var(get_val_(IDENTIFIER, name)),
        wrap_str_lit(" = malloc("),
        wrap_int(arr_len),
        wrap_str_lit(")")
      ));
  } else {
    if (init == 0) init = new_ast0(INTEGER, 0);
    append_glo_decl(comp_assignment(name, init));
  }
}

void comp_assignment_constant(text constant_name, ast rhs) {
  append_glo_decl(string_concat3(constant_name, wrap_char('='), comp_rvalue(rhs)));
}

// Enums are just like global variables, but they are readonly.
// Since anything that's not a local variable is considered global, this makes
// it easy to implement enums.
void comp_enum_cases(ast ident, ast cases) {
  ast cas;
  if (ident != 0) {
    append_glo_decl(string_concat3(wrap_str_lit("#_ "), wrap_str_pool(get_val_(IDENTIFIER, ident)), wrap_str_lit(" enum declaration")));
  } else {
    append_glo_decl(wrap_str_lit("#_ Enum declaration"));
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
    append_glo_decl(string_concat3(wrap_str_lit("#_ "), wrap_str_pool(get_val_(IDENTIFIER, ident)), wrap_str_lit(" struct member declarations")));
  } else {
    append_glo_decl(wrap_str_lit("#_ Struct member declarations"));
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

  // Open init block if not already opened
  if (op != FUN_DECL) {
    // In AWK, assignments outside functions don't do anything, only assignments
    // in functions have effect. Therefore, we wrap global variable declarations
    // in setup functions, that are called at the start of main, with each
    // setup function calling the previous one before initializing its own
    // variables, so that variables are initialized in the correct order.
    if (!init_block_open) {
      init_block_open = true;
      init_block_id += 1;
      append_glo_decl(string_concat3(
        wrap_str_lit("function setup_"),
        wrap_int(init_block_id),
        wrap_str_lit("() {")
      ));
      if (init_block_id > 1) {
        append_glo_decl(string_concat3(
          wrap_str_lit("  setup_"),
          wrap_int(init_block_id - 1),
          wrap_str_lit("()")
        ));
      }
      nest_level += 1;
    }
  } else {
    // Close init block if opened
    if (init_block_open) {
      init_block_open = false;
      nest_level -= 1;
      append_glo_decl(wrap_str_lit("}\n"));
    }
  }

  if (op == DECLS) { // Variable declarations
    // AUTO_KW and REGISTER_KW can simply be ignored. STATIC_KW is the default
    // storage class for global variables since pnut-sh only supports 1
    // translation unit.
#ifdef SUPPORT_TYPE_SPECIFIERS
    if (get_child_(DECLS, node, 1) == EXTERN_KW) fatal_error("Extern storage class specifier not supported");
#endif
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


// Required codegen interface functions
void codegen_begin() {
  print_awk_shebang();
  print_awk_comment("Generated by pnut AWK backend");
  putchar('\n');
}

void codegen_glo_decl(ast decl) {
#ifndef ONE_PASS_GENERATOR_NO_EARLY_OUTPUT
  // Reset text and glo decls buffers
  glo_decl_ix = 0;
  text_alloc = 1;
#endif

  // Reset local environment
  cgc_locals = cgc_locals_fun = 0;
  cgc_fs = 1; // 1 to account for the return location parameter

  comp_glo_decl(decl);
#ifndef ONE_PASS_GENERATOR_NO_EARLY_OUTPUT
  print_glo_decls();
#endif

#ifdef PRINT_MEMORY_STATS
  // Statistics
  max_text_alloc = max_text_alloc > text_alloc ? max_text_alloc : text_alloc;
  cumul_text_alloc += text_alloc;
#endif
}

void codegen_end() {
  // Output main runtime
  putstr("function malloc(size) {\n");
  putstr("  return (__ALLOC += size) - size;\n");
  putstr("}\n");
  putstr("\n");
  putstr("BEGIN {\n");
  putstr("  __ALLOC=1 # Allocation pointer\n");
  putstr("  _main()\n");
  putstr("  exit 0\n");
  putstr("}\n");
}
