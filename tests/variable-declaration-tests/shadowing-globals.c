int x = 1, y, z = 2;

void testShadowingGlobals() {
  int x, z = 3;
  y = 4;
  putchar(x + 48);
  putchar(10);
  putchar(y + 48);
  putchar(10);
  putchar(z + 48);
  putchar(10);
}

int main() {
  testShadowingGlobals();
  putchar(x + 48);
  putchar(10);
  putchar(y + 48);
  putchar(10);
  putchar(z + 48);
  putchar(10);

  return 0;
}