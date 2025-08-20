/*
 * cp.c: Copy the contents of one file to another
 *
 * Warning: This is a very minimal implementation of the 'cp' command.
 * It does __not__ propagate file permissions.
 *
 * Usage: ./cp.sh <source> <destination>
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#ifndef ENTRY_POINT
#define ENTRY_POINT main
#endif

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

#define BUF_SIZE 1024

int ENTRY_POINT(int argc, char **argv) {
  int src, dst;
  int len;
  char buffer[BUF_SIZE];

  if (argc != 3) {
    printf("Usage: cp <source> <destination>\n");
    return 1;
  }

  // TODO: Propagate permissions from source file
  src = open(argv[1], O_RDONLY);
  dst = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0644);

  if (src == -1) { file_error(argv[1]); }
  if (dst == -1) { file_error(argv[2]); }

  while ((len = read(src, buffer, BUF_SIZE)) != 0) {
    write(dst, buffer, len);
  }
}
