// Utility in C that reads a file and does nothing with it

void main(int argc, char **argv) {
  int fd;
  int c;

  fd = fopen(argv[1], "r");

  while ((c = fgetc(fd)) != -1) {
    putchar(c);
  }
}
