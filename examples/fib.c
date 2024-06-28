int fib(int n) {
  int t1, t2;
  if (n < 2) {
    return n;
  } else {
    t1 = fib(n - 1);
    t2 = fib(n - 2);
    return t1 + t2;
  }
}

void main() {
  int n;
  n = fib(20);
  printf("fib(20) = %d\n", n);
}
