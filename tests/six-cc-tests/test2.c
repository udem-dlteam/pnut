int square(int x) {
  return x*x;
}

void main()
{
  int n;

  /* print a number to stdout */

  int p;
  int digit;

  n = square(10);
  p = 1;

  while (p * 10 <= n) p = p * 10;

  while (p > 0) {
    digit = n / p;
    putchar(48 + digit);
    n = n % p;
    p = p / 10;
  }

  putchar(10);
}
