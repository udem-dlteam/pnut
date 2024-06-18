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

int main() {
  int a;
  int b;
  int x;
  int y;
  int z;

  x = 3;
  y = x++;
  x = 3;
  z = ++x;

  a = 0;
  b = 0;
  while (a < 10) {
    b += a++;
  }
  //printf("a: %d, b: %d, x: %d, y: %d, z: %d\n", a, b, x, y, z);
  putstring("a: ");
  putnumber(a);
  putchar(',');
  putchar(' ');
  putstring("b: ");
  putnumber(b);
  putchar(',');
  putchar(' ');
  putstring("x: ");
  putnumber(x);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putnumber(y);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putnumber(z);
  putchar(10);
  return 0;
}
