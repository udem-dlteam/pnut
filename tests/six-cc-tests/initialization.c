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

void main() {
  int i = 28;
  int j = 14;
  int k = i + j;
  putstring("i: ");
  putnumber(i);
  putstring(", j: ");
  putnumber(j);
  putstring(", k: ");
  putnumber(k);
  putchar('\n');
  return 0;
}
