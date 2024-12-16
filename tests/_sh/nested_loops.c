int foo() {
  int i, j;
  for (i = 0; i < 10; i++) {
    for (j = 0; j < 10; j++) {
      if (i == j && i == 5) {
        return i + j;
      }
    }
  }
}

void main() {
  printf("%d\n", foo());
}
