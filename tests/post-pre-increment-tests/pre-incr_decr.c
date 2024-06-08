void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void simplePreIncrementAndDecrement() {
  int x = 0;
  putstring("x: ");
  putchar(x + 48);
  putchar(10);
  putchar(++x + 48);
  putchar(10);
  putchar(x + 48);
  putchar(10);
  putchar(--x + 48);
  putchar(10);
  putchar(x + 48);
  putchar(10);
}

void complexPreIncrementAndDecrement() {
  int x = 0;
  int y = 0;

  putstring("x: ");
  putchar(x + 48);
  putchar(10);
  putchar(++x + 48);
  putchar(10);
  putchar(x + 48);
  putchar(10);
  putstring("y: ");
  putchar(y + 48);
  putchar(10);
  putchar(++y + 48);
  putchar(10);
  putchar(y + 48);
  putchar(10);
  putchar(--y + 48);
  putchar(10);
  putchar(y + 48);
  putchar(10);
  putstring("x: ");
  putchar(--x + 48);
  putchar(10);
  putchar(x + 48);
  putchar(10);
}

int main() {
  simplePreIncrement();
  complexPreIncrement();
  return 0;
}
