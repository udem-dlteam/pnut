
void testLocalMultipleVariableDeclaration() {
  int a;
  int b, c;
  a = 1;
  b = 2;
  c = 3;

  putchar(a+48);
  putchar(10);
  putchar(b+48);
  putchar(10);
  putchar(c+48);
  putchar(10);
}

void testLocalMultipleVariableDeclarationInitialization() {
  int a = 1, b = 2, c = 3;

  putchar(a+48);
  putchar(10);
  putchar(b+48);
  putchar(10);
  putchar(c+48);
  putchar(10);
}

int main() {
  testLocalMultipleVariableDeclaration();
  testLocalMultipleVariableDeclarationInitialization();
  return 0;
}