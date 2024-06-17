void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}


int fib(int n) {
  int n2;
  int n3;
  if (n <= 1) {
    return n;
  }

  n2 = fib(n - 1);
  n3 = fib(n - 2);
  n2 = n2 + n3;
  return n2;
}

int main() {
  int n123;
  n123 = fib(15);
  if (n123 == 610){
    putstring("610");
  } else {
    putchar(88);
  }
  putchar(10);
  return 0;
}
