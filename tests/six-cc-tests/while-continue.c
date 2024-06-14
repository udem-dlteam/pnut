//putstring function
void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int main() {
  int i = 0;
  putstring("test\n");
  while (i < 35) {
    /* Skip over even numbers*/
    if (i % 2 == 0) {
      //continue; continue not supported?
      putstring("even\n");
    }
    putchar(i + 48);
    putchar('\n');
    if (i % 3 == 0) {
      putstring("fizz");
    }
    if (i % 5 == 0) {
      putstring("buzz");
    }
    putchar('\n');
    if (i == 12){
      putstring("break\n");
      break;// NOTE: BUG HERE!! This line is not executed without the line above
      // Instead of breaking the loop unexpected behavior occurs ie: exit code 1; elf header dump
      // When putchar is used before break the exit code gets a strange value (not 0) this changes based on character and characters put before break
      // If putstring is used then putchar, the same behavior occurs
      // If putchar is used then putstring, the exit code is 0
    }
    i = i + 1;
  }
}
