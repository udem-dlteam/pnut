
void testLocalMultipleVariableDeclaration() {
  int a, b, c;
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

int main() {
  testLocalMultipleVariableDeclaration();
  return 0;
}