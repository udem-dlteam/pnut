void fgetcTest() {
  int f;
  char c;
  f = fopen("tests/primitives-tests/primitives-test.c", 0);
  while ((c = fgetc(f)) != -1) {
    putchar(c);
  }
  fclose(f);
}

int main() {
  interlacedGetCharFgetc();
  fgetcTest();
  return 0;
}