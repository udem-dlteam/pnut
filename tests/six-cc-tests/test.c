void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void main() {
  /* print a number to stdout */
  int NL;
  int ZERO;
  int n;
  int p;
  int digit;
  NL = 10;
  ZERO = 48;
  n = 31416;
  p = 1;

  while (p * 10 <= n) p = p * 10;

  while (p > 0) {
    digit = n / p;
    putchar(ZERO + digit);
    n = n % p;
    p = p / 10;
  }

  if (NL == 31416){
    putstring("31416");
  }
  putchar(10);
  return 0;
}
