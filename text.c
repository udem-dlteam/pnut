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

text wrap_str_pool(const int ident_symbol) {
  return wrap_str_imm(symbol_buf(ident_symbol), 0);
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
