void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int ack(int m, int n)
{
  int a; /* Local variable so that the function is not simple */
  if (m == 0) return n + 1;
  if(m > 0 && n == 0) return ack(m - 1, 1);
  return ack(m - 1, ack(m, n - 1));
}

int main() {
  int m = 3;
  int n = 3;
  putstring("ack(");
  putchar(48 + m);
  putstring(", ");
  putchar(48 + n);
  putstring(") = ");
  int acked = ack(m, n);
  putchar(acked); //'=' == 61
  return 0;
}
