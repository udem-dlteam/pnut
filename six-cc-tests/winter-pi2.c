/* #include <stdio.h> */

/* https://cs.uwaterloo.ca/~alopez-o/math-faq/mathtext/node12.html */

int r[2801];
int i;
int k;
int b;
int d;
int c = 0;

int main() {
  int newline;
  int newline2;
  newline = identity(10, 2, 3);

  for (; i < 2800; i++) {
    r[i] = 2000;
  }
  r[i] = 0;

  for (k = 280; k > 0; k = k - 14) {

    d = 0;
    i = k;
    for (;;) {
      d = d + r[i] * 10000;
      b = 2 * i - 1;

      r[i] = d % b;
      d = d / b;
      i--;
      if (i == 0) break;
      d = d * i;
    }
    putchar(48 + (c + d / 10000) / 1000 % 10);
    putchar(48 + (c + d / 10000) / 100 % 10);
    putchar(48 + (c + d / 10000) / 10 % 10);
    putchar(48 + (c + d / 10000) % 10);
    c = d % 10000;
  }

  putchar(newline);

  return 0;
}

int identity(int x, int y, int z) {
  return x;
}
