int hash(char_ptr s) {
  int hash;
  int ix;
  hash = 0;
  ix = 0;
  while (s[ix] != 0) {
    hash = s[ix] + (hash << 6) + (hash << 16) - hash;
    hash = hash & 65535; /* Most shells have 32-bit integers, so we have to make sure we don't "overflow" */
    ix++;
  }
  return ix;
}

int main() {
  int MAX_SIZE;
  int_ptr f;
  char_ptr s;
  int len;
  int h;
  MAX_SIZE = 200;
  s = malloc(MAX_SIZE);
  f = open("six-cc-tests/close.c", 0);
  len = read(f, s, MAX_SIZE - 1);
  s[len] = 0;
  close(f);
  printf("Read content: %s\n", s);
  printf("File descriptor: %s\n", f);
  printf("Quote: \"\n", len);
  printf("Backslash: \\\n", len);
  printf("Read len: %d\n", len);
  printf("Read result: %.*s\n", len, s);
  printf("hash: %d\n", hash(s));
}
