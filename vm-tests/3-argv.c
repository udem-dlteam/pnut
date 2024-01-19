// args: helloworld
// sdbm algorithm
// Found http://www.cse.yorku.ca/~oz/hash.html
int hash(char *s) {
  int hash, ix;
  hash = 0;
  ix = 0;
  while (s[ix] != 0) {
    // printf("%d\n", ix);
    hash = s[ix] + (hash << 6) + (hash << 16) - hash;
    hash = hash & 0xffff; // Most shells have 32-bit integers, so we have to make sure we don't "overflow"
    ix++;
  }
  return ix;
}

int string_len(char *s) {
  int ix;
  ix = 0;
  while (s[ix] != 0) {
    ix++;
  }
  return ix;
}

int main(int argc, char **argv) {
  return string_len(argv[1]) + hash(argv[1]);
}
