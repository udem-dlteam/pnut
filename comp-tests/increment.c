int main() {
  int x;
  int y;
  int z;

  x = 3;
  y = x++;
  x = 3;
  z = ++x;
  printf("x: %d, y: %d, z: %d\n", x, y, z);
}
