int square(int x) {
  int y = x*x;
  return y;
}

void main()
{
  int n;
  int p;
  int digit;

  n = square(10);
  p = 1;

  while (p * 10 <= n){
    p = p * 10;
  }

  while (p > 0) {
    digit = n / p;
    putchar(48 + digit);
    n = n % p;
    p = p / 10;
  }

  putchar(10);
  return 0;
}
