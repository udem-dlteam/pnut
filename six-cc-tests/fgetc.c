int main() {
  int f;
  char c;
  f = fopen("six-cc-tests/fgetc.c", 0);
//  putchar(f+48);
  while ((c = fgetc(f)) != -1) {
    putchar(c);
  }
  fclose(f);
}
