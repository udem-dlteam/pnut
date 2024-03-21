/* input:c4.c */
/* Compare stdin with file, output matching prefix */
int main() {
  char c1;
  char c2;
  FILE_ptr f;
  f = fopen("c4.c", 0);
  while ((c1 = getchar()) && (c2 = fgetc(f)) && c1 == c2 && c1 != EOF) {
    putchar(c1);
  }
}
