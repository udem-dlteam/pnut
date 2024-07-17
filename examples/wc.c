// wc utility in C. Count lines, words, and characters of files passed as arguments, or stdin if no arguments are passed.

// #define is_word_separator(c) ((c) == ' ' || (c) == '\n' || (c) == '\t')

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#define BUF_SIZE 1024

char buf[BUF_SIZE];

int is_word_separator(char c) {
  return c == ' ' || c == '\n' || c == '\t';
}

void wc_fd(int fd, char *filename) {
  int lines = 0, words = 0, chars = 0;
  int sep = 0, last_sep = 0;
  int i;
  int n = BUF_SIZE;

  while (n == BUF_SIZE) {
    n = read(fd, buf, BUF_SIZE);

    i = 0;
    while (i < n) {
      chars += 1;
      if (buf[i] == '\n') lines += 1;

      sep = is_word_separator(buf[i]);
      if (sep && !last_sep) {
        words += 1;
      }
      last_sep = sep;
      i += 1;
    }
  }

  if (filename != 0) {
    printf("%d %d %d %s\n", lines, words, chars, filename);
  } else {
    printf("%d %d %d\n", lines, words, chars);
  }
}

void wc_file(char *filename) {
  int fd = open(filename, 0);
  if (fd < 0) exit(1);
  wc_fd(fd, filename);
  close(fd);
}

int main(int argc, char **argv) {

  int i;

  if (argc >= 2) {
    for (i=1; i<argc; ++i) {
      wc_file(argv[i]);
    }
  } else {
    wc_fd(0, 0);
  }

  return 0;
}
