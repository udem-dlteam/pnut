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
  int i;
  while (i < 15) {
    n = fib(i);
    printf("fib(%d) = %d\n", i, n);
    ++i;
  }
}
