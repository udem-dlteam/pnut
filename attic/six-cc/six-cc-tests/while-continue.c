int main() {
  for (i = 0; i < 35; i++) {
    /* Skip over even numbers*/
    if (i % 2 == 0) {
      continue;
    }
    printf("%d: ", i);
    if (i % 3 == 0) {
      printf("fizz");
    }
    if (i % 5 == 0) {
      printf("buzz");
    }
    printf(".\n");
    if (i == 12) break;
  }
}
