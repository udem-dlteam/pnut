void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void main() {
  int i = 28;
  int j = 14;
  int k = i + j;
  //printf("i: %d, j: %d, k: %d\n", i, j, k);
  putstring("i: ");
  putchar(i + 48);
  putstring(", j: ");
  putchar(j + 48);
  putstring(", k: ");
  putchar(k + 48);
  putchar('\n');
}
