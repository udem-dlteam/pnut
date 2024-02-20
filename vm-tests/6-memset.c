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
  printf("%d %d %d %d %d \n", s2[0], s2[1], s2[2], s2[3], s2[4]); // Print first 5 bytes before memset
  memset(s2, 'A', 4); // Replace "Test" with AAAA
  printf("%d %d %d %d %d \n", s2[0], s2[1], s2[2], s2[3], s2[4]); // Print first 5 bytes after memset
  printf("%s\n", s2);
  return 0;
}
