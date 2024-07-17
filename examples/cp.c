// cp utility in C. Copies the content of the first file to the second file.

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#ifndef O_WRONLY
#define O_WRONLY 1
#endif

void file_error(char *filename) {
  printf("cp: %s: no such file or directory\n", filename);
  exit(1);
}

#define BUFFER_LEN 1024
char* buffer[BUFFER_LEN];

int main(int argc, char **args) {
  int src, dst;
  char c;
  int len;

  if (argc != 3) {
    printf("Usage: cp <source> <destination>\n");
    return 1;
  }

  src = open(args[1], O_RDONLY);
  dst = open(args[2], O_WRONLY);

  if (src == 0) { file_error(args[1]); }
  if (dst == 0) { file_error(args[2]); }

  while ((len = read(src, buffer, BUFFER_LEN)) != 0) {
    write(dst, buffer, len);
  }
}
