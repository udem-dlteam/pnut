enum Category() {
  LOWERCASE = 1;
  UPPERCASE;
  NEWLINE;
  OTHER;
}

/* For all ascii characters, categorize them into 4 categories */
int categorize_char(char c) {
  int a;
  switch (c) {
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
      a = LOWERCASE;
      return LOWERCASE;
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
    case 'H':
    case 'I':
    case 'J':
    case 'K':
    case 'L':
    case 'M':
    case 'N':
    case 'O':
    case 'P':
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
    case 'U':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
      return UPPERCASE;
    case '\n':
      return NEWLINE;
    default:
      return OTHER;
  }
  /* Dead code, but puts the switch in non-tail call position */
  return a;
}

char_ptr category_to_string(int category) {
  switch (category) {
    case LOWERCASE:
      return "lowercase";
    case UPPERCASE:
      return "uppercase";
    case NEWLINE:
      return "newline";
    case OTHER:
      return "other";
  }
  return "unknown";
}

int main() {
  FILE_ptr f;
  char c;
  f = fopen("six-cc-tests/fgetc.c", 0);
  while ((c = fgetc(f)) != EOF) {
    printf("'%c' = %d: %d\n", c, c, categorize_char(c));
  }
}
