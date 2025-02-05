
#ifdef HANDLE_SIGNALS
#include <signal.h>

void signal_callback_handler(int signum) {
  if (signum == SIGINT){
    printf("Caught signal %d\n",signum);
    printf("Tokenizer at %s:%d:%d\n", fp_filepath, line_number, column_number);
    exit(1);
  }
}
#endif

void print_string_char(int c) {
  if (c == 7)       putstr("\\a");
  else if (c == 8)  putstr("\\b");
  else if (c == 12) putstr("\\f");
  else if (c == 10) putstr("\\n");
  else if (c == 13) putstr("\\r");
  else if (c == 9)  putstr("\\t");
  else if (c == 11) putstr("\\v");
  else if (c == '\\' || c == '\'' || c == '"') { putchar('\\'); putchar(c); }
  else if (c < 32 || c > 126) { putchar('\\'); putint(c >> 6); putint((c >> 3) & 7); putint(c & 7); }
  else putchar(c);
}

void print_tok_string(int string_probe) {
  char *string_start = STRING_BUF(string_probe);
  char *string_end = string_start + STRING_LEN(string_probe);

  while (string_start < string_end) {
    print_string_char(*string_start);
    string_start += 1;
  }
}

int print_tok_indent_level = 0;
int print_tok_preceding_nl_count = 0;
void print_tok_indent() {
  int i;
  for (i = 0; i < print_tok_indent_level; i += 1) putchar(' ');
}

void print_tok(int tok, int val) {
  int i;

  // print_tok treats '{', '}' and '\n' specially:
  // - '{' increases the indent level by 2
  // - '}' decreases the indent level by 2
  // - '\n' prints a newline and increments print_tok_preceding_nl_count

  // When print_tok_preceding_nl_count is not 0, print_tok_indent is called
  // before printing the token This ensures that tokens are properly indented
  // after a newline.

  if (tok == '\n') {
    if (print_tok_preceding_nl_count >= 2) return; // Skip consecutive newlines
    print_tok_preceding_nl_count += 1;
    putchar('\n');
    return;
  } else if (tok == '{') {
    print_tok_indent();
    putchar(tok);
    print_tok_indent_level += 2;
    return;
  } else if (tok == '}') {
    print_tok_indent_level -= 2;
    print_tok_indent();
    putchar(tok);
    return;
  }

  if (print_tok_preceding_nl_count != 0) {
    print_tok_indent();
    print_tok_preceding_nl_count = 0;
  }

  if      (tok == AUTO_KW)      putstr("auto");
  else if (tok == BREAK_KW)     putstr("break");
  else if (tok == CASE_KW)      putstr("case");
  else if (tok == CHAR_KW)      putstr("char");
  else if (tok == CONST_KW)     putstr("const");
  else if (tok == CONTINUE_KW)  putstr("continue");
  else if (tok == DEFAULT_KW)   putstr("default");
  else if (tok == DO_KW)        putstr("do");
  else if (tok == DOUBLE_KW)    putstr("double");
  else if (tok == ELSE_KW)      putstr("else");
  else if (tok == ENUM_KW)      putstr("enum");
  else if (tok == EXTERN_KW)    putstr("extern");
  else if (tok == FLOAT_KW)     putstr("float");
  else if (tok == FOR_KW)       putstr("for");
  else if (tok == GOTO_KW)      putstr("goto");
  else if (tok == IF_KW)        putstr("if");
  else if (tok == INT_KW)       putstr("int");
  else if (tok == LONG_KW)      putstr("long");
  else if (tok == REGISTER_KW)  putstr("register");
  else if (tok == RETURN_KW)    putstr("return");
  else if (tok == SHORT_KW)     putstr("short");
  else if (tok == SIGNED_KW)    putstr("signed");
  else if (tok == SIZEOF_KW)    putstr("sizeof");
  else if (tok == STATIC_KW)    putstr("static");
  else if (tok == STRUCT_KW)    putstr("struct");
  else if (tok == SWITCH_KW)    putstr("switch");
  else if (tok == TYPEDEF_KW)   putstr("typedef");
  else if (tok == UNION_KW)     putstr("union");
  else if (tok == UNSIGNED_KW)  putstr("unsigned");
  else if (tok == VOID_KW)      putstr("void");
  else if (tok == VOLATILE_KW)  putstr("volatile");
  else if (tok == WHILE_KW)     putstr("while");

  else if (tok == AMP_AMP)      putstr("&&");
  else if (tok == AMP_EQ)       putstr("&=");
  else if (tok == ARROW)        putstr("->");
  else if (tok == BAR_BAR)      putstr("||");
  else if (tok == BAR_EQ)       putstr("|=");
  else if (tok == CARET_EQ)     putstr("^=");
  else if (tok == EQ_EQ)        putstr("==");
  else if (tok == GT_EQ)        putstr(">=");
  else if (tok == LSHIFT_EQ)    putstr("<<=");
  else if (tok == LSHIFT)       putstr("<<");
  else if (tok == LT_EQ)        putstr("<=");
  else if (tok == MINUS_EQ)     putstr("-=");
  else if (tok == MINUS_MINUS)  putstr("--");
  else if (tok == EXCL_EQ)      putstr("!=");
  else if (tok == PERCENT_EQ)   putstr("%=");
  else if (tok == PLUS_EQ)      putstr("+=");
  else if (tok == RSHIFT_EQ)    putstr(">>=");
  else if (tok == RSHIFT)       putstr(">>");
  else if (tok == SLASH_EQ)     putstr("/=");
  else if (tok == STAR_EQ)      putstr("*=");
  else if (tok == HASH_HASH)    putstr("##");
  else if (tok == PLUS_PLUS_PRE)    putstr("++");
  else if (tok == MINUS_MINUS_PRE)  putstr("--");
  else if (tok == PLUS_PLUS_POST)   putstr("++");
  else if (tok == MINUS_MINUS_POST) putstr("--");

  else if (tok == IDENTIFIER) {
    putstr(string_pool + heap[val+1]);
  } else if (tok == MACRO) {
    putchar('[');
    putstr(string_pool + heap[val+1]);
    putchar(']');
  }  else if (tok == INTEGER) {
    putint(-val);
  } else if (tok == CHARACTER) {
    putchar('\'');
    print_string_char(val);
    putchar('\'');
  } else if (tok == STRING) {
    putchar('"');
    print_tok_string(val);
    putchar('"');
  } else if (tok == MACRO_ARG) {
    putstr("ARG["); putint(val); putstr("]");
  } else {
    putchar(tok);
  }

  if (tok != '\n') putchar(' ');
}

// Show the type of a token.
// This is used for showing more helpful error messages.
void print_tok_type(int tok) {

  if      (tok == AUTO_KW)      putstr("auto");
  else if (tok == BREAK_KW)     putstr("break");
  else if (tok == CASE_KW)      putstr("case");
  else if (tok == CHAR_KW)      putstr("char");
  else if (tok == CONST_KW)     putstr("const");
  else if (tok == CONTINUE_KW)  putstr("continue");
  else if (tok == DEFAULT_KW)   putstr("default");
  else if (tok == DO_KW)        putstr("do");
  else if (tok == DOUBLE_KW)    putstr("double");
  else if (tok == ELSE_KW)      putstr("else");
  else if (tok == ENUM_KW)      putstr("enum");
  else if (tok == EXTERN_KW)    putstr("extern");
  else if (tok == FLOAT_KW)     putstr("float");
  else if (tok == FOR_KW)       putstr("for");
  else if (tok == GOTO_KW)      putstr("goto");
  else if (tok == IF_KW)        putstr("if");
  else if (tok == INT_KW)       putstr("int");
  else if (tok == LONG_KW)      putstr("long");
  else if (tok == REGISTER_KW)  putstr("register");
  else if (tok == RETURN_KW)    putstr("return");
  else if (tok == SHORT_KW)     putstr("short");
  else if (tok == SIGNED_KW)    putstr("signed");
  else if (tok == SIZEOF_KW)    putstr("sizeof");
  else if (tok == STATIC_KW)    putstr("static");
  else if (tok == STRUCT_KW)    putstr("struct");
  else if (tok == SWITCH_KW)    putstr("switch");
  else if (tok == TYPEDEF_KW)   putstr("typedef");
  else if (tok == UNION_KW)     putstr("union");
  else if (tok == UNSIGNED_KW)  putstr("unsigned");
  else if (tok == VOID_KW)      putstr("void");
  else if (tok == VOLATILE_KW)  putstr("volatile");
  else if (tok == WHILE_KW)     putstr("while");

  else if (tok == AMP_AMP)      putstr("&&");
  else if (tok == AMP_EQ)       putstr("&=");
  else if (tok == ARROW)        putstr("->");
  else if (tok == BAR_BAR)      putstr("||");
  else if (tok == BAR_EQ)       putstr("|=");
  else if (tok == CARET_EQ)     putstr("^=");
  else if (tok == EQ_EQ)        putstr("==");
  else if (tok == GT_EQ)        putstr(">=");
  else if (tok == LSHIFT_EQ)    putstr("<<=");
  else if (tok == LSHIFT)       putstr("<<");
  else if (tok == LT_EQ)        putstr("<=");
  else if (tok == MINUS_EQ)     putstr("-=");
  else if (tok == MINUS_MINUS)  putstr("--");
  else if (tok == EXCL_EQ)      putstr("!=");
  else if (tok == PERCENT_EQ)   putstr("%=");
  else if (tok == PLUS_EQ)      putstr("+=");
  else if (tok == RSHIFT_EQ)    putstr(">>=");
  else if (tok == RSHIFT)       putstr(">>");
  else if (tok == SLASH_EQ)     putstr("/=");
  else if (tok == STAR_EQ)      putstr("*=");
  else if (tok == HASH_HASH)    putstr("##");
  else if (tok == PLUS_PLUS_PRE)    putstr("++");
  else if (tok == MINUS_MINUS_PRE)  putstr("--");
  else if (tok == PLUS_PLUS_POST)   putstr("++");
  else if (tok == MINUS_MINUS_POST) putstr("--");
  else if (tok == IDENTIFIER)       putstr("identifier");
  else if (tok == INTEGER)          putstr("integer");
  else if (tok == CHARACTER)        putstr("character");
  else if (tok == STRING)           putstr("string");
  else if (tok == MACRO)            putstr("macro");
  else if (tok == MACRO_ARG)        putstr("macro argument");
  else if (tok == EOF)              putstr("end of file");
  else if (tok == '\n')             putstr("newline");
  else if (' ' < tok && tok < 127)  { putchar('\''); putchar(tok); putchar('\''); }
  else {
    printf("tok=%d\n", tok);
    fatal_error("print_tok_type: unknown token");
  }
}

void ast_to_sexp(ast obj);

void type_ast_to_sexp(ast type) {
  printf("type");
}

void ast_list_to_sexp(ast obj) {
  while (obj != 0) {
    ast_to_sexp(get_child_(',', obj, 0));
    obj = get_child_opt_(',', ',', obj, 1);
    if (obj != 0) putchar(' '); // Separate elements with a space
  }
}

void ast_to_sexp(ast obj) {
  if (obj == 0) return;

  int i = 0;
  int nb_children = get_nb_children(obj);
  int op = get_op(obj);

  // Except for terminal objects which don't have children, we print the
  // operator and value of the object before printing its children.
  switch (op) {
    case IDENTIFIER:
      putstr(STRING_BUF(get_val_(IDENTIFIER, obj)));
      break;

    case STRING:
      putchar('"');
      putstr(STRING_BUF(get_val_(STRING, obj)));
      putchar('"');
      break;

    case INTEGER:
      putint(-get_val_(INTEGER, obj));
      break;

    case CHARACTER:
      // If printable ASCII: print as character, otherwise print as octal
      putchar('\'');
      print_string_char(get_val_(CHARACTER, obj));
      putchar('\'');
      break;

    case DECLS:
      // For clarity, we print the declarations without a parent `DECLS` node
      ast_list_to_sexp(get_child_opt_(DECLS, ',', obj, 0));
      break;

    case TYPEDEF_KW:
      printf("(typedef ");
      ast_list_to_sexp(get_child_opt_(TYPEDEF_KW, ',', obj, 0));
      printf(")");
      return;

    case DECL:
      // Nodes of type DECL are a bit special because they contain a type, and types have their own structure
      putstr("(decl ");
      ast_to_sexp(get_child__(DECL, IDENTIFIER, obj, 0));
      putchar(' ');
      type_ast_to_sexp(get_child_(DECL, obj, 1));
      if (get_child_(DECL, obj, 2) != 0) {
        putchar(' ');
        ast_to_sexp(get_child_(DECL, obj, 2));
      }
      printf(")");
      break;

    case CAST:
      printf("(cast ");
      type_ast_to_sexp(get_child_(DECL, get_child__(CAST, DECL, obj, 0), 1)); // Get type out of decl
      printf(" ");
      ast_to_sexp(get_child_(CAST, obj, 1));
      printf(")");
      break;

    case SIZEOF_KW:
      printf("(sizeof ");
      if (get_op(get_child_(SIZEOF_KW, obj, 0)) == DECL) {
        type_ast_to_sexp(get_child_(DECL, get_child_(SIZEOF_KW, obj, 0), 1));
      } else {
        ast_to_sexp(get_child_(SIZEOF_KW, obj, 0));
      }
      printf(")");
      break;

    default:
      putchar('(');
      print_tok_type(op);
      putchar(' ');
      for (; i < get_nb_children(obj); i += 1) {
        ast_to_sexp(get_child(obj, i));
        if (get_child(obj, i) != 0 && i < get_nb_children(obj) - 1) putchar(' ');
      }
      putchar(')');
      break;
  }
}

// void show_struct(ast struct_type) {
//   ast members = get_child(canonicalize_type(struct_type), 2);

//   char* name = string_pool + get_val(get_val(get_child(struct_type, 1)));

//   printf("##### Struct %s #####\n", name);
//   printf("sizeof(%s) = %d\n", name, struct_size(struct_type));

//   while (get_op(members) == ',') {
//     printf("%s = obj[%d]\n", string_pool + get_val(get_val(get_child(members, 0))), struct_member_offset(struct_type, get_child(members, 0)));
//     members = get_child(members, 2);
//   }
// }

void print_tokens(int tokens) {
  while (tokens != 0) {
    print_tok(car(car(tokens)), cdr(car(tokens)));
    tokens = cdr(tokens);
  }
}

void print_macro_args(int args) {
  int arg;
  if (args != 0) {
    print_macro_args(cdr(args));
    print_tokens(car(args));
    if (cdr(args) != 0) putchar(',');
  }
}

void print_macro_ctx(int ix, int ident, int tokens, int args) {
  int arg;
  if (ident == 0) {
    printf("# %-3d: <unnamed>", ix);
  } else {
    printf("# %-3d: %s", ix, STRING_BUF(ident));
  }
  if (args) {
    putchar('(');
    print_macro_args(args);
    putchar(')');
  }
  if (tokens != 0) {
    printf(" -> ");
    print_tokens(tokens);
  }
}

void print_macro_stack() {
  int i = 0;
  putstr("\n######### macro_stack: #########\n");
  while (3 * i < macro_stack_ix) {
    print_macro_ctx(i, macro_stack[i * 3 + 2], macro_stack[i * 3], macro_stack[i * 3 + 1]);
    putchar('\n');
    i += 1;
  }
  print_macro_ctx(i, macro_ident, macro_tok_lst, macro_args);
  putstr("\n################################\n");
}
