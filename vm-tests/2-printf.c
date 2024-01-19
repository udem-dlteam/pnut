// args:
int string_len(char *s) {
  int ix;
  ix = 0;
  while (s[ix] != 0) {
    ix++;
  }
  return ix;
}

int main(int argc, char **argv) {
  char* s, *s2, *s3;
  s = "Bonhomme";
  s2 = "Carnaval";
  s3 = "Sept...\nheures";
  printf("Hello, %s %s %s\n", s, s2, s3);
  printf("Hello, %.*s\n", 2, s);
  printf("Hello, %.*s\n", 5, s+3);
  printf("Hello, %c%c%c%c%c\n", s[3], s[4], s[5], s[6], s[7]);
  printf("Hello, %x%x%x%x%x\n", s[3], s[4], s[5], s[6], s[7]);
  printf("%d\n", -1);
  printf("%8.4s123\n", "A");
  printf("%8.4s123\n", "ABCDEF");
  printf("string_len(%s) = %d\n", s, string_len(s));
  return 0;
}
