#include <stdio.h>

#ifdef PNUT_CC
typedef int FILE;
#endif

int emit_line(int line, FILE *f) {
  char c;
  putchar(line+ 48);
  putchar(':');
  putchar(' ');
  while ((c = fgetc(f)) && c != -1 && c != '\n') {
    putchar(c);
  }
  if (c != -1) {
    putchar('\n');
  }
  return c;
}

int main() {
  char c1;
  char c2;
  int i = 0;
  FILE *f1 = fopen("tests/_all/six-cc-tests/fgetc.c", "r");
  FILE *f2 = fopen("tests/_all/six-cc-tests/while-fun-call.c", "r");
  while (1) {
    c1 = emit_line(i, f1);
    c2 = emit_line(i, f2);
    if (c1 == -1 || c2 == -1){
      break;
    }
    i += 1;
  }

  fclose(f1);
  fclose(f2);
  return 0;
}
