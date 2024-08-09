int x, y, z;

void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void testGlobalMultipleVariableDeclaration() {
  x = 4;
  y = 5;
  z = 6;

  putstring("Global Variables:\n");
  putchar(x+48);
  putchar(10);
  putchar(y+48);
  putchar(10);
  putchar(z+48);
  putchar(10);
}

int a = 4, b = 5, c = 6;

void testGlobalMultipleVariableDeclarationInitialization() {
  putstring("Global Variables Initialized:\n");
  putchar(a+48);
  putchar(10);
  putchar(b+48);
  putchar(10);
  putchar(c+48);
  putchar(10);
}

int main() {
  testGlobalMultipleVariableDeclaration();
  testGlobalMultipleVariableDeclarationInitialization();
  return 0;
}