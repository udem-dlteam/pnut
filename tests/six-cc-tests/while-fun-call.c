void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int emit_line(int line, int f) {
  char c;
  putstring("Line: ");
  putchar(line+ 48);
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
  f1 = fopen("six-cc-tests/fgetc.c", 0);
  f2 = fopen("six-cc-tests/while-fun-call.c", 0);
  while (1) {
    c1 = emit_line(i, f1);
    c2 = emit_line(i, f2);
    if (c1 == -1 || c2 == -1){
      putstring("");
      break// NOTE: BUG HERE!! This line is not executed without the line above
      // Instead of breaking the loop unexpected behavior occurs ie: exit code 1; elf header dump
      // When putchar is used before break the exit code gets a strange value (not 0) this changes based on character and characters put before break
      // If putstring is used then putchar, the same behavior occurs
      // If putchar is used then putstring, the exit code is 0
    }
    i += 1;
  }
}
