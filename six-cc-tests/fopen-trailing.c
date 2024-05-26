int main() {
  FILE_ptr f;
  char c;
  f = fopen("six-cc-tests/no-trailing.txt", "r");
  while ((c = fgetc(f)) != EOF) {
    putchar(c);
  }
}
