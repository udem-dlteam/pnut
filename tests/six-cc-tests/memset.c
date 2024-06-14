void putstring(char * s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int main() {
  int MAX_SIZE;
  char * s;
  char * s2;
  char * f;
  int len;
  int res;
  int i;
  MAX_SIZE = 100;
  malloc(MAX_SIZE);
  s = malloc(MAX_SIZE);
  s2 = malloc(MAX_SIZE);
  f = open("tests/six-cc-tests/memset.c", 0);
  len = read(f, s, MAX_SIZE - 1);
  s[len] = 0;
  /* Initialize s2 with s */
  for (i = 0; i < len; i++) {
    s2[i] = s[i];

  }
  putstring("s = s2: ");
  putchar(memcmp(s, s2, len));
  s[12] += 25;
  putstring("s[12] = 25: ");
  putchar(memcmp(s, s2, len));
}
