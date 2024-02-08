int main(int argc, char_ptr argv) {
  int MAX_SIZE;
  char_ptr s;
  char_ptr s2;
  char_ptr f;
  int len;
  int res;
  MAX_SIZE = 100;
  s = malloc(MAX_SIZE);
  s2 = malloc(MAX_SIZE);
  f = open("comp-tests/memset.c", 0);
  len = read(f, s, MAX_SIZE - 1);
  s[len] = 0;
  s[0] = 12;
  memset(s, 25, len);
  res = memcmp(s, s2, len);
  printf("%d\n", res);
}
