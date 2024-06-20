void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void putnumber(int n) {
  int acc = 0;
  int i = 0;
  int *digits = malloc(10 * sizeof(int)); // Dynamically allocate memory for digits
  
  if (digits == 0) {
    putstring("Memory allocation failed\n");
    return;
  }

  if (n == 0) {
    putchar(48);
    free(digits); // Free allocated memory
    return;
  }
  if (n < 0) {
    putchar('-');
    n = -n;
  }
  while (n > 0) {
    digits[i] = n % 10;
    n = n / 10;
    i++;
  }
  i--;
  while (i >= 0) {
    putchar(digits[i] + 48);
    i--;
  }
  
  free(digits); // Free allocated memory
}

int main() {
  int x;
  int y;
  int z;

  x = 42;
  y = 128;

  z = ++x;
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
  z = --x;
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
  z = x++;
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
  z = x--;
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
  z = x += y;
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
  z = x -= y;
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
  z = x *= y;
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
  z = x /= y;
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
  z = x %= y;
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
  z = x &= y;
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
  z = x |= y;
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
  z = x ^= y;
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
  z = x <<= y;
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
  z = x >>= y;
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
  z = ~z;
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
  z = +z;
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
  z = 1 ? x : y;
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
  z = 0 ? x : y;
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
