int main() {
  int n123;
  n123 = 0;
  print_int_reverse(25);
  putchar(10);
  n123 = fib(20);
  print_int_reverse(n123);
  putchar(10);
}

int fib(int n) {
  int n2;
  int n3;
  n2 = 0;
  n3 = 0;
  if (n <= 1) {
    return n;
  }

  n2 = fib(n - 1);
  n3 = fib(n - 2);
  n2 = n2 + n3;
  return n2;
}

void print_int_reverse(int n) {
  while (n > 0) {
    putchar(48 + n % 10);
    n = n / 10;
  }
}
