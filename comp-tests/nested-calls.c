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
  r[abc(123)] = 2000;
  /* while (counter = decrement(counter)) { */
  while (counter--) {
    printf("Counter: %d\n", counter);
  }
  exit(abc(def(123)));
  return abc(123);
}
