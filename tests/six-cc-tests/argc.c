/* args: abc def hij */
void putstring(char * s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int main(int argc, char ** args) {
  int i = 0;
  putstring("Number of arguments: ");
  putchar(48 + argc);
  putchar('\n');
  putstring("Arguments:\n");
  while (i < argc) {
    putstring("argv[");
    putchar(48 + i);
    putstring("] = ");
    putstring(args[i]);
    putchar('\n');
    i = i + 1;
  }
}
