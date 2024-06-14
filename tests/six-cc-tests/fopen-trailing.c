int main() {
  int f;
  char c;
  f = fopen("six-cc-tests/no-trailing.txt", 0);
  while ((c = fgetc(f)) != -1) {
    putchar(c);
  }
}
