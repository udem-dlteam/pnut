int x = 1, y, z = 2;

void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void testShadowingGlobals() {
  int x = 0, z = 3;
  y = 4;
  putchar(x + 48);
  putchar(10);
  putchar(y + 48);
  putchar(10);
  putchar(z + 48);
  putchar(10);
}

int main() {
  putstring("Test Shadowing Globals:\n");
  testShadowingGlobals();
  putstring("Global Variables:\n");
  putchar(x + 48);
  putchar(10);
  putchar(y + 48);
  putchar(10);
  putchar(z + 48);
  putchar(10);

  return 0;
}