void putint_aux(int n) {
  if (n >= 10) putint_aux(n / 10);
  putchar('0' + (n % 10));
}

void putint(int n) {
  if (n < 0) {
    putchar('-');
    putint_aux(-n);
  } else {
    putint_aux(n);
  }
}

int fib(int n) {
  if (n <= 1) return n;

  return fib(n - 1) + fib(n - 2);
}

int main() {
  putint(fib(15));
  putchar(10);
  return 0;
}
