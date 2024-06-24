/* Test that enum variables can be shadowed by local bindings */

void putstring(const char *s) {
  while (*s) {
    putchar(*s);
    s++;
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

enum ChildEnum { VAL, NEXT, LL_SIZE };

enum ParentEnum { VAL2, NEXT2, LL_SIZE2 };

struct TestStruct {
  int VAL;
  int NEXT;
  int LL_SIZE;
};

void shadow(int NEXT) {
  int VAL;
  VAL = 123;
  NEXT = 456;
  putstring("VAL: ");
  putnumber(VAL);
  putstring(", NEXT: ");
  putnumber(NEXT);
  putchar(10);
}

int main() {
  putstring("VAL: ");
  putnumber(VAL); // Note: This refers to the global enum 'VAL'
  putstring(", NEXT: ");
  putnumber(NEXT); // Note: This refers to the global enum 'NEXT'
  putchar(10);
  shadow(789);
  putstring("VAL: ");
  putnumber(VAL); // Note: This refers to the global enum 'VAL'
  putstring(", NEXT: ");
  putnumber(NEXT); // Note: This refers to the global enum 'NEXT'
  putchar(10);
  return 0;
}