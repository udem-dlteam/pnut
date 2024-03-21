/* Test that the lazy evaluation properties of || and && are respected */

void DO_NOT_CALL(int x) {
  int a; /* Local variable so that the function is not simple */
  printf("Boom!");
  exit(1);
  return x;
}

void SHOULD_BE_CALLED(int x) {
  int a; /* Local variable so that the function is not simple */
  if (x) {
    printf("Ah!\n");
  }
  else {
    printf("Oh!\n");
  }
  return x;
}

int main() {
  if (1 || DO_NOT_CALL(1)) {
    printf("This should print 1\n");
  }
  if (0 || SHOULD_BE_CALLED(1)) {
    printf("This should print 2\n");
  }
  if (0 || SHOULD_BE_CALLED(0)) {
    printf("This should not print\n");
  }
  if (1 && SHOULD_BE_CALLED(1)) {
    printf("This should print 3\n");
  }
  if (1 && SHOULD_BE_CALLED(0)) {
    printf("This should not print\n");
  }
  if (0 && DO_NOT_CALL(1)) {
    printf("This should not print\n");
  }
}
