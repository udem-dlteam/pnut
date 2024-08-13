int emit_line(int line, int f) {
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
  int f1;
  int f2;
  char c1;
  char c2;
  int i = 0;
  f1 = fopen("tests/_all/six-cc-tests/fgetc.c", "r");
  f2 = fopen("tests/_all/six-cc-tests/while-fun-call.c", "r");
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
