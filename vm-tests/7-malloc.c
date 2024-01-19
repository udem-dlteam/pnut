// args:
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
  char *s, *s2, *s3;
  int SIZE, h;
  SIZE = 100;
  s  = malloc(SIZE);
  s2 = malloc(SIZE);
  s3 = malloc(SIZE);
  memset(s, 'A', SIZE / 2);
  memset(s + (SIZE / 2), 'Z', SIZE / 2);
  s[SIZE - 1] = 0;
  memset(s2, 'a', SIZE / 2);
  memset(s2 + (SIZE / 2), 'z', SIZE / 2);
  s2[SIZE - 1] = 0;
  memset(s3, 'a', SIZE / 2);
  memset(s3 + (SIZE / 2), 'z', SIZE / 2);
  s3[SIZE - 1] = 0;
  h = hash(s) + hash(s2) + hash(s3);
  free(s); free(s2);
  return h;
}
