//putstring function
void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void putnumber(int n) {
  int acc = 0;
  int i = 0;
  int digits[10];
  
  if (n == 0) {
    putchar(48);
    return;
  }
  
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

int main() {
  int i = 0;
  while (i < 35) {
    /* Skip over even numbers*/
    if (i % 2 == 0) {
      i = i + 1;
      continue;
    }
    putnumber(i);
    putstring(": ");
    if (i % 3 == 0) {
      putstring("fizz");
    }
    if (i % 5 == 0) {
      putstring("buzz");
    }
    putstring(".\n");
    if (i == 12){
      break;
    }
    i = i + 1;
  }
}
