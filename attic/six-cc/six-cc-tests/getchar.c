/* input:six-cc-tests/getchar-interlaced.c */
int main() {
  char c;
  while ((c = getchar()) != EOF) {
    putchar(c);
  }
}
