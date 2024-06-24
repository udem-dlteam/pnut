int main() {
  int f;
  char c;
  int i = 0;
  while (i < 100) {
    f = fopen("tests/six-cc-tests/fgetc.c", 0);
    while ((c = fgetc(f)) != -1) {
      putchar(c);
    }
    putchar('\n');
    fclose(f);
    i = i + 1;
  }
}
