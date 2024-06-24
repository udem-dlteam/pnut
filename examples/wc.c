// wc utility in C. Read from stdin and count lines, words, and characters.

// #define is_word_separator(c) ((c) == ' ' || (c) == '\n' || (c) == '\t')

int is_word_separator(char c) {
  return c == ' ' || c == '\n' || c == '\t';
}

void main() {
  int lines = 0, words = 0, chars = 0;
  char c;
  int sep = 0, last_sep = 0;

  while ((c = getchar()) != -1) {
    chars += 1;
    if (c == '\n') lines += 1;

    sep = is_word_separator(c);
    if (sep && !last_sep) {
      words += 1;
    }
    last_sep = sep;
  }

  printf("%d %d %d\n", lines, words, chars);
}
