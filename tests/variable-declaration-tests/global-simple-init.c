int x = 4, y = 5, z = 6;

void testGlobalMultipleVariableDeclarationInitialization() {
  putchar(x+48);
  putchar(10);
  putchar(y+48);
  putchar(10);
  putchar(z+48);
  putchar(10);
}

int main() {
  testGlobalMultipleVariableDeclarationInitialization();
  return 0;
}