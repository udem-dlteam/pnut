void putstring(char_ptr s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void main() {
  int i = 0;
  int j = 5;
  int k = 12;
  char * p = "_hello";

  if ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
  {
    //printf("p: %c\n", *p);
    putstring("p: ");
    putchar(*p);
    putchar('\n');
  }
  if ((*p >= 'a' || *p <= 'z') && (*p >= 'A' || *p <= 'Z') && !(*p >= '0' || *p <= '9') && *p == '_')
  {
    //printf("p: %c\n", *p);
    putstring("p: ");
    putchar(*p);
    putchar('\n');
  }
}
