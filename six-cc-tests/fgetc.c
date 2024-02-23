int main() {
  int_ptr f;
  char c;
  f = fopen("six-cc-tests/fgetc.c", 0);
  c = fgetc(f);
  while (c) {
    putchar(c);
    c = fgetc(f);
  }
}
