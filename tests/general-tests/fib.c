int fib(int n) {
  if (n < 2) {
    return n;
  }
  return fib(n - 1) + fib(n - 2);
}

int main() {
  int n;
  n = fib(15);
  if(n != 610) {
    putchar('X');
  }
  else {
    putchar('O');
  }
  return 0;
}
