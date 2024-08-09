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
  putstring("i: ");
  putchar(i + 48);
  putstring(", j: ");
  putchar(j + 48);
  putstring(", k: ");
  if(k == 42) {
    putstring("42");
  } else {
    putstring("not 42");
  }
  putchar('\n');
  putstring("t: ");
  putstring(t);
  putchar('\n');

  return 0;
}
