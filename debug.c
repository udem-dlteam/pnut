void print_string_char(int c) {
  if (c == 7)       putstr("\\a");
  else if (c == 8)  putstr("\\b");
  else if (c == 12) putstr("\\f");
  else if (c == 10) putstr("\\n");
  else if (c == 13) putstr("\\r");
  else if (c == 9)  putstr("\\t");
  else if (c == 11) putstr("\\v");
  else if ((c == '\\') OR (c == '\'') OR (c == '"')) {
    putchar('\\'); putchar(c);
  } else if ((c < 32) OR (c > 126)) {
    putchar('\\'); putint(c >> 6); putint((c >> 3) & 7); putint(c & 7);
  }
  else putchar(c);
}

void print_tok(int tok, int val) {

  int i;

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
  else if (tok == ERROR_KW)     putstr("error");
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
  else if (tok == BAR_BAR)      putstr("||");
  else if (tok == BAR_EQ)       putstr("|=");
  else if (tok == CARET_EQ)     putstr("^=");
  else if (tok == EQ_EQ)        putstr("==");
  else if (tok == GT_EQ)        putstr(">=");
  else if (tok == LSHIFT_EQ)    putstr("<<=");
  else if (tok == LSHIFT)       putstr("<<");
  else if (tok == LT_EQ)        putstr("<=");
  else if (tok == MINUS_EQ)     putstr("-=");
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
    i = 0;
    while (string_pool[val+i] != 0) {
      print_string_char(string_pool[val+i]);
      i += 1;
    }
    putchar('"');
  } else if (tok == MACRO_ARG) {
    putstr("ARG["); putint(val); putstr("]");
  } else {
    putchar(tok);
  }

  if (tok == ';') { // Simple heuristic to print newlines. This makes the output more readable.
    putchar(' ');
    putchar('\n');
  } else {
    putchar(' ');
  }
}
