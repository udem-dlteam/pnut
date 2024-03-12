int main() {
  int_ptr f1;
  int_ptr f2;
  char c1;
  char c2;
  f1 = fopen("six-cc-tests/fgetc.c", 0);
  f2 = fopen("six-cc-tests/while-fun-call.c", 0);
  while ((c1 = fgetc(f1)) && (c2 = fgetc(f2)) && c1 != 0 && c2 != 0) {
    putchar(c1);
    putchar(c2);
  }
}
