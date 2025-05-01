// On mksh, this test takes more than 30s. Adjusting the timeout to make it pass.
// timeout: 120

#include <stdio.h>

#ifdef PNUT_CC
typedef int FILE;
#endif

int main() {
  FILE *f;
  char c;
  int i = 0;
  while (i < 100) {
    f = fopen("tests/_all/six-cc-tests/fgetc.c", "r");
    while ((c = fgetc(f)) != -1) {
      putchar(c);
    }
    putchar('\n');
    fclose(f);
    i = i + 1;
  }
}
