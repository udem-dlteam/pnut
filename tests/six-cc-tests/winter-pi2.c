/* #include <stdio.h> */

/* https://cs.uwaterloo.ca/~alopez-o/math-faq/mathtext/node12.html */

void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}


int r[2801];
int i;
int k = 280;
int b;
int d;
int c = 0;

int main() {
  int newline;
  int newline2;
  newline = identity(10, 2, 3);

  int i = 0;
  while (i < 2800) {
    r[i] = 2000;
    i = i + 1;
  }
  r[i] = 0;


  while (k > 0) {

    d = 0;
    i = k;

    while (1) {
      d = d + r[i] * 10000;
      b = 2 * i - 1;

      r[i] = d % b;
      d = d / b;
      i = i - 1;
      if (i == 0){
        break;
      }
      d = d * i;
    }
    putchar(48 + (c + d / 10000) / 1000 % 10);
    putchar(48 + (c + d / 10000) / 100 % 10);
    putchar(48 + (c + d / 10000) / 10 % 10);
    putchar(48 + (c + d / 10000) % 10);
    c = d % 10000;
    k = k - 14;
  }

  putchar(newline);

  return 0;
}

int identity(int x, int y, int z) {
  return x;
}
