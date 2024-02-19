/* args: abc def hij */
int main(int argc, char_ptr argv) {
  int x;
  int y;
  int z;

  x = 42;
  y = 128;

  z = ++x;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = --x;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = x++;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = x--;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = x += y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = x -= y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = x *= y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = x /= y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = x %= y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
}
