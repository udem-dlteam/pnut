int abs(int number)
{
  if(number < 0) return -number;
  return number;
}

int even(int number)
{
  int a; /* Local variable so that the function is not simple */
  if(number == 0) return 1;
  return odd(abs(number)-1);
}

int odd(int number)
{
  int a; /* Local variable so that the function is not simple */
  if( number == 0 ) return 0;
  return even(abs(number)-1);
}

void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int main() {
  int n1;
  int n2;
  n1 = even(10);
  n2 = odd(10);
//  printf("n1 = %d\n", n1);
//  printf("n2 = %d\n", n2);
  putstring("n1 = ");
  putchar(n1);
  putchar('\n');
  putstring("n2 = ");
  putchar(n2);
  putchar('\n');
  return 0;
}
