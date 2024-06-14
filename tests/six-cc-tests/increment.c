int main() {
  int a;
  int b;
  int x;
  int y;
  int z;

  x = 3;
  y = x++;
  x = 3;
  z = ++x;

  a = 0;
  b = 0;
  while (a < 10) {
    b += a++;
  }
  printf("a: %d, b: %d, x: %d, y: %d, z: %d\n", a, b, x, y, z);
}
