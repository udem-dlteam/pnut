enum Category {
  LOWERCASE = 1,
  UPPERCASE,
  NEWLINE,
  OTHER
};

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

void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int main() {
  int f;
  char c;
  f = fopen("tests/six-cc-tests/fgetc.c", 0);
  while ((c = fgetc(f)) != -1) {
    putstring("'");
    putchar(c);
    putstring("' = ");
    putchar(c);
    putstring(": ");
    putchar(categorize_char(c) + 48);
    putchar('\n');
  }
}
