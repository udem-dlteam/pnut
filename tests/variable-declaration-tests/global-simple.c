int x, y, z;

void testGlobalMultipleVariableDeclaration() {
  x = 4;
  y = 5;
  z = 6;
  putchar(x+48);
  putchar(10);
  putchar(y+48);
  putchar(10);
  putchar(z+48);
  putchar(10);
}

int main() {
  testGlobalMultipleVariableDeclaration();
  return 0;
}