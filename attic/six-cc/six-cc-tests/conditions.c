void main() {
  int i = 0;
  int j = 5;
  int k = 12;
  char_ptr p = "_hello";

  if ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
  {
    printf("p: %c\n", *p);
  }
  if ((*p >= 'a' || *p <= 'z') && (*p >= 'A' || *p <= 'Z') && !(*p >= '0' || *p <= '9') && *p == '_')
  {
    printf("p: %c\n", *p);
  }
}
