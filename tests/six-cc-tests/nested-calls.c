int counter = 10;

int r[2801];

void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void putnumber(int n) {
  int acc = 0;
  int i = 0;
  int *digits = malloc(10 * sizeof(int)); // Dynamically allocate memory for digits

  if (digits == 0) {
    putstring("Memory allocation failed\n");
    return;
  }

  if (n == 0) {
    putchar(48);
    free(digits); // Free allocated memory
    return;
  }

  while (n > 0) {
    digits[i] = n % 10;
    n = n / 10;
    i++;
  }
  i--;
  while (i >= 0) {
    putchar(digits[i] + 48);
    i--;
  }

  free(digits); // Free allocated memory
}

int abc(int x) {
  return x + 1;
}

int def(int x) {
  return x * 2;
}

int hij(int x, int y) {
  return x * y;
}

int main() {
  int result;
  r[abc(123)] = 2000;
  result = r[abc(123)];

  putnumber(result);
  putchar('\n');

  while (counter-- > 0) {
    putstring("Counter: ");
    putnumber(counter);
    putchar('\n');
  }

  result = abc(42);
  putnumber(result);
  putchar('\n');
  result = abc(def(123));
  putnumber(result);
  putchar('\n');
  result = hij(abc(123), def(123));
  putnumber(result);
  putchar('\n');
  result = hij(abc(123), def(123)) + abc(123);
  putnumber(result);
  putchar('\n');
  result = hij(2, 3);
  putnumber(result);
  putchar(' ');
  result = abc(123);
  putnumber(result);
  putchar('\n');

  return 0;
}
