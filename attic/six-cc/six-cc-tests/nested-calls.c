int abc(int x) {
  return x + 1;
}

int def(int x) {
  return x * 2;
}

int hij(int x, int y) {
  return x * y;
}

int counter;
counter = 10;

int decrement(int i) {
  i--;
  return i;
}

int r[2801];

int main() {
  abc(123);
  r[abc(123)] = 2000;
  printf("%d\n", r[abc(123)]);
  /* while (counter = decrement(counter)) { */
  while (counter--) {
    printf("Counter: %d\n", counter);
  }
  printf("%d\n", abc(42)); /* 43 */
  printf("%d\n", abc(def(123))); /* (123 * 2) + 1 */
  printf("%d\n", hij(abc(123), def(123))); /* (123 + 1) * (123 * 2) = 30504 */
  printf("%d\n", hij(abc(123), def(123)) + abc(123)); /* 30504 + 124 = 30628 */
  printf("%d %d\n", hij(2, 3), abc(123)); /* 6 124 */
}
