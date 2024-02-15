int abc(int x) {
  return x + 1;
}

int def(int x) {
  return x * 2;
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
  printf("%d \n", r[abc(123)]);
  /* while (counter = decrement(counter)) { */
  while (counter--) {
    printf("Counter: %d\n", counter);
  }
  return abc(def(123));
}
