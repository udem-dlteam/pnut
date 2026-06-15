#include <stdio.h>

#ifdef PNUT_CC
typedef int FILE;
#endif

int main() {
  FILE *f;
  int c;
  f = fopen("tests/_all/six-cc-tests/no-trailing.txt", "r");
  while ((c = fgetc(f)) != -1) {
    putchar(c);
  }
}
