int square(int x) {
  return x*x;
}

void main()
{
  int n = square(10);

  /* print a number to stdout */

  int p = 1;

  while (p * 10 <= n) p *= 10;

  while (p > 0) {
    int digit = n / p;
    putchar(48 + digit);
    n %= p;
    p /= 10;
  }

  putchar(10);
}
