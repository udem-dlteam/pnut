// args:
int string_len(char *s) {
  int ix;
  ix = 0;
  while (*s++ != 0) ix++;
  return ix;
}

void string_copy(char *dst, char *src) {
  while (*src != 0) {
    *dst = *src;
    src++;
    dst++;
  }
  *dst = 0;
}

int main(int argc, char **argv) {
  char *s, *s2, *s3;
  int len;
  s = "Test string";
  len = string_len(s);
  s2 = malloc(len + 1);
  s3 = malloc(len + 1);
  string_copy(s2, s);
  string_copy(s3, s2);
  s3[2] = s3[2] + 12;
  printf("memcmp(s2, s) = %d, memcmp(s3, s) = %d\n", memcmp(s2, s, len), memcmp(s3, s, len));
  return 0;
}
