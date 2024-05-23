
void print_string_char(int c) {
  if (c == 7) printf("\\a");
  else if (c == 8) printf("\\b");
  else if (c == 12) printf("\\f");
  else if (c == 10) printf("\\n");
  else if (c == 13) printf("\\r");
  else if (c == 9) printf("\\t");
  else if (c == 11) printf("\\v");
  else if ((c == '\\') OR (c == '\'') OR (c == '"')) printf("\\%c", c);
  else if ((c < 32) OR (c > 126)) printf("\\%d%d%d", c>>6, (c>>3)&7, c&7);
  else putchar(c);
}

void print_tok(int tok, int val) {

  int i;

  if (tok == AUTO_KW) printf("auto");
  else if (tok == BREAK_KW) printf("break");
  else if (tok == CASE_KW) printf("case");
  else if (tok == CHAR_KW) printf("char");
  else if (tok == CONST_KW) printf("const");
  else if (tok == CONTINUE_KW) printf("continue");
  else if (tok == DEFAULT_KW) printf("default");
  else if (tok == DO_KW) printf("do");
  else if (tok == DOUBLE_KW) printf("double");
  else if (tok == ELSE_KW) printf("else");
  else if (tok == ENUM_KW) printf("enum");
  else if (tok == ERROR_KW) printf("error");
  else if (tok == EXTERN_KW) printf("extern");
  else if (tok == FLOAT_KW) printf("float");
  else if (tok == FOR_KW) printf("for");
  else if (tok == GOTO_KW) printf("goto");
  else if (tok == IF_KW) printf("if");
  else if (tok == INT_KW) printf("int");
  else if (tok == LONG_KW) printf("long");
  else if (tok == REGISTER_KW) printf("register");
  else if (tok == RETURN_KW) printf("return");
  else if (tok == SHORT_KW) printf("short");
  else if (tok == SIGNED_KW) printf("signed");
  else if (tok == SIZEOF_KW) printf("sizeof");
  else if (tok == STATIC_KW) printf("static");
  else if (tok == STRUCT_KW) printf("struct");
  else if (tok == SWITCH_KW) printf("switch");
  else if (tok == TYPEDEF_KW) printf("typedef");
  else if (tok == UNION_KW) printf("union");
  else if (tok == UNSIGNED_KW) printf("unsigned");
  else if (tok == VOID_KW) printf("void");
  else if (tok == VOLATILE_KW) printf("volatile");
  else if (tok == WHILE_KW) printf("while");

  else if (tok == AMP_AMP) printf("&&");
  else if (tok == AMP_EQ) printf("&=");
  else if (tok == BAR_BAR) printf("||");
  else if (tok == BAR_EQ) printf("|=");
  else if (tok == CARET_EQ) printf("^=");
  else if (tok == EQ_EQ) printf("==");
  else if (tok == GT_EQ) printf(">=");
  else if (tok == LSHIFT_EQ) printf("<<=");
  else if (tok == LSHIFT) printf("<<");
  else if (tok == LT_EQ) printf("<=");
  else if (tok == MINUS_EQ) printf("-=");
  else if (tok == EXCL_EQ) printf("!=");
  else if (tok == PERCENT_EQ) printf("%=");
  else if (tok == PLUS_EQ) printf("+=");
  else if (tok == RSHIFT_EQ) printf(">>=");
  else if (tok == RSHIFT) printf(">>");
  else if (tok == SLASH_EQ) printf("/=");
  else if (tok == STAR_EQ) printf("*=");
  else if (tok == HASH_HASH) printf("##");

  else if (tok == IDENTIFIER) {
    printf("%s", string_pool + heap[val+1]);
  } else if (tok == MACRO) {
    printf("[%s]", string_pool + heap[val+1]);
  }  else if (tok == INTEGER) {
    printf("%d", -val);
  } else if (tok == CHARACTER) {
    printf("'%d''", val);
    print_string_char(val);
    printf("' ");
  } else if (tok == STRING) {
    printf("\"");
    i = 0;
    while (string_pool[val+i] != 0) {
      print_string_char(string_pool[val+i]);
      i += 1;
    }
    printf("\"");
  } else if (tok == MACRO_ARG) {
    printf("ARG[%d]", val);
  } else {
    printf("%c", tok);
  }

  if (tok == ';') { // Simple heuristic to print newlines. This makes the output more readable.
    printf(" \n");
  } else {
    putchar(' ');
  }
}
