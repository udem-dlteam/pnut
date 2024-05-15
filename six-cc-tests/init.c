void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void main() {
  int i = 0;
  int j = 5;
  int k;
  char * s;
  char * t = "Hello, world!";
  k = 42;
//  printf("i: %d, j: %d, k: %d\n", i, j, k);
//  printf("t: %s\n", t);
  putstring("i: ");
  putchar(i + 48);
  putstring(", j: ");
  putchar(j + 48);
  putstring(", k: ");
  putchar(k + 48);
  putchar('\n');
  putstring("t: ");
  putstring(t);
  putchar('\n');

  return 0;
}
