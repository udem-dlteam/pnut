int main() {
  int f;
  char c;
  f = fopen("tests/six-cc-tests/fgetc.c", 0);
  while ((c = fgetc(f)) != -1) {
    putchar(c);
  }
  fclose(f);
}