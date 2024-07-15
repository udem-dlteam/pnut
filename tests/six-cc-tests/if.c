void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int main() {
  int x;
  int y;

  x = 42;
  y = 128;

  if (x < y) {
    //printf("if (x < y)\n");
    putstring("if (x < y)\n");
  } else {
    //printf("else (x >= y)\n");
    putstring("else (x >= y)\n");
  }

  if (x > y) {
    //printf("x > y\n");
    putstring("x > y\n");
  } else if (x == y) {
    //printf("else if (x == y)\n");
    putstring("else if (x == y)\n");
  }

  if (x > y) {
    //printf("if (x > y)\n");
    putstring("if (x > y)\n");
  } else if (x == y) {
    //printf("else if (x == y)\n");
    putstring("else if (x == y)\n");
  } else {
    //printf("else (x < y)\n");
    putstring("else (x < y)\n");
  }

  return 0;
}
