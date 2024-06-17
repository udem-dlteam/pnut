/* Test that enum variables can be shadowed by local bindings */

void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void putnumber(int n) {
  if (n == 0) {
    putchar(48);
    return;
  }
  int acc = 0;
  int i = 0;
  int digits[10];
  while (n > 0) {
    digits[i] = n % 10;
    n = n / 10;
    i = i + 1;
  }
  i = i - 1;
  while (i >= 0) {
    putchar(digits[i] + 48);
    i = i - 1;
  }
}

enum ChildEnum { VAL, NEXT, LL_SIZE };

enum ParentEnum { VAL, NEXT, LL_SIZE, VAL2, NEXT2, LL_SIZE2 };

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
  putnumber(VAL);
  putstring(", NEXT: ");
  putnumber(NEXT);
  putchar(10);
  shadow(789);
  putstring("VAL: ");
  putnumber(VAL);
  putstring(", NEXT: ");
  putnumber(NEXT);
  putchar(10);
  return 0;
}
