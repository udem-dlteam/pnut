void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int main() {
  int x;
  int y;
  int z;

  x = 42;
  y = 128;

  z = ++x;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = --x;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  putchar(10);  
  z = x++;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = x--;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = x += y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = x -= y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);  
  z = x *= y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = x /= y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = x %= y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = x &= y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = x |= y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = x ^= y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = x <<= y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = x >>= y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = ~z;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = +z;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = 1 ? x : y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
  z = 0 ? x : y;
  putstring("x: ");
  putchar(x + 48);
  putchar(',');
  putchar(' ');
  putstring("y: ");
  putchar(y + 48);
  putchar(',');
  putchar(' ');
  putstring("z: ");
  putchar(z + 48);
  putchar(10);
}