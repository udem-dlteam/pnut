int main(int argc, char_ptr argv) {
  int MAX_SIZE;
  char_ptr s;
  char_ptr s2;
  char_ptr f;
  int len;
  int res;
  int i;
  MAX_SIZE = 100;
  malloc(MAX_SIZE);
  s = malloc(MAX_SIZE);
  s2 = malloc(MAX_SIZE);
  f = open("six-cc-tests/memset.c", 0);
  len = read(f, s, MAX_SIZE - 1);
  s[len] = 0;
  /* Initialize s2 with s */
  for (i = 0; i < len; i++) {
    s2[i] = s[i];

  }
  printf("s = s2: %d\n", memcmp(s, s2, len));
  s[12] += 25;
  printf("s[12] = 25: %d\n", memcmp(s, s2, len));
}
