// args: vm-tests/8-open.c
// sdbm algorithm
// Found http://www.cse.yorku.ca/~oz/hash.html
int hash(char *s) {
  int hash, ix;
  hash = 0;
  ix = 0;
  while (s[ix] != 0) {
    hash = s[ix] + (hash << 6) + (hash << 16) - hash;
    hash = hash & 0xffff; // Most shells have 32-bit integers, so we have to make sure we don't "overflow"
    ix++;
  }
  return ix;
}

int main(int argc, char **argv) {
  int MAX_SIZE;
  int *f;
  char *s;
  int len;
  MAX_SIZE = 1000;
  s = malloc(MAX_SIZE);
  f = open(argv[1], 0);
  len = read(f, s, MAX_SIZE);
  close(f);
  printf("%d\n", len);
  printf("%.*s\n", len, s);
  printf("hash: %d\n", hash(s));
  return len;
}
