int main() {
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
  z = x &= y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = x |= y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = x ^= y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = x <<= y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = x >>= y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = ~z;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = +z;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = 1 ? x : y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
  z = 0 ? x : y;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
}
