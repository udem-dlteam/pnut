void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void putnumber(int n) {
  int acc = 0;
  int i = 0;
  int digits[10];

  if (n == 0) {
    putchar(48);
    return;
  }
  
  while (n > 0) {
    digits[i] = n % 10;
    n = n / 10;
    i = i + 1;
  }
  i = i - 1;
  while (i >= 0) {
    putchar(digits[i] + 48);
    i = i - 1;
  }
}

void main() {
  int i = 28;
  int j = 14;
  int k = i + j;
  putstring("i: ");
  putnumber(i);
  putstring(", j: ");
  putnumber(j);
  putstring(", k: ");
  putnumber(k);
  putchar('\n');
  return 0;
}
