/* Test that the lazy evaluation properties of || and && are respected */

void putstr(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void DO_NOT_CALL(int x) {
  int a; /* Local variable so that the function is not simple */
  putstr("Boom!");
  exit(1);
  return x;
}

void SHOULD_BE_CALLED(int x) {
  int a; /* Local variable so that the function is not simple */
  if (x) {
    putstr("Ah!\n");
  }
  else {
    putstr("Oh!\n");
  }
  return x;
}

int main() {
  if (1 || DO_NOT_CALL(1)) {
    putstr("This should print 1\n");
  }
  if (0 || SHOULD_BE_CALLED(1)) {
    putstr("This should print 2\n");
  }
  if (0 || SHOULD_BE_CALLED(0)) {
    putstr("This should not print\n");
  }
  if (1 && SHOULD_BE_CALLED(1)) {
    putstr("This should print 3\n");
  }
  if (1 && SHOULD_BE_CALLED(0)) {
    putstr("This should not print\n");
  }
  if (0 && DO_NOT_CALL(1)) {
    putstr("This should not print\n");
  }
}
