int fib(int n) {
  if (n < 2) {
    return n;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

void main() {
  int n;
  n = fib(20);
  printf("fib(20) = %d\n", n);
}
