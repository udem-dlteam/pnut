int main() {
  FILE_ptr f;
  char c;
  int i = 0;
  for (i = 0; i < 100; i++) {
    f = fopen("six-cc-tests/fgetc.c", "r");
    while ((c = fgetc(f)) != EOF) {
      putchar(c);
    }
    fclose(f);
  }
}
