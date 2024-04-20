int emit_line(int line, FILE_ptr f) {
  char c;
  printf("%d: ", line);
  while ((c = fgetc(f)) && c != EOF && c != '\n') {
    putchar(c);
  }
  if (c != EOF) {
    putchar('\n');
  }
  return c;
}

int main() {
  FILE_ptr f1;
  FILE_ptr f2;
  char c1;
  char c2;
  int i = 0;
  f1 = fopen("six-cc-tests/fgetc.c", "r");
  f2 = fopen("six-cc-tests/while-fun-call.c", "r");
  while (1) {
    c1 = emit_line(i, f1);
    c2 = emit_line(i, f2);
    if (c1 == EOF || c2 == EOF) break;
    i += 1;
  }
}
