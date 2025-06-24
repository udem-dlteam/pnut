/*
 * cat.c: Output the contents of files passed as arguments or stdin
 *
 * Usage: ./cat.sh <files>
 *        ./cat.sh < input
 */

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

#define BUF_SIZE 1024

char buf[BUF_SIZE];

void cat_fd(int fd) {
  int n = BUF_SIZE;
  while (n == BUF_SIZE) {
    n = read(fd, buf, BUF_SIZE);
    if (n < 0 || write(1, buf, n) != n) exit(1);
  }
}

void cat_file(char *filename) {
  int fd = open(filename, 0);
  if (fd < 0) exit(1);
  cat_fd(fd);
  close(fd);
}

int main(int argc, char **myargv) {

  int i;

  if (argc >= 2) {
    for (i=1; i<argc; ++i) {
      if (myargv[i][0] == '-' && myargv[i][1] == '\0') {
        cat_fd(0);
      } else {
        cat_file(myargv[i]);
      }
    }
  } else {
    cat_fd(0);
  }

  return 0;
}
