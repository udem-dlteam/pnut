/* args: abc def hij */
void putstring(char * s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int main(int argc, char * args) {
  int i = 0;
  //printf("Number of arguments: %d\n", argc);
  putstring("Number of arguments: ");
  putchar(48 + argc);
  putchar('\n');
  //printf("Arguments:\n");
  putstring("Arguments:\n");
  while (i < argc) {
    //printf("argv[%d] = %s\n", i, args[i]);
    putstring("argv[");
    putchar(48 + i);
    putstring("] = ");
    putstring(args[i]);
    putchar('\n');
    i = i + 1;
  }
}
