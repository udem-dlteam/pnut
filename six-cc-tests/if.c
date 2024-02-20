int main(int argc, char_ptr argv) {
  int x;
  int y;

  x = 42;
  y = 128;

  if (x < y) {
    printf("if (x < y)\n");
  } else {
    printf("else (x >= y)\n");
  }

  if (x > y) {
    printf("x > y\n");
  } else if (x == y) {
    printf("else if (x == y)\n");
  }

  if (x > y) {
    printf("if (x > y)\n");
  } else if (x == y) {
    printf("else if (x == y)\n");
  } else {
    printf("else (x < y)\n");
  }
}
