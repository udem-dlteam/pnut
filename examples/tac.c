#include <stdio.h>

void main() {
  char* buf = malloc(1024);
  char* buf_start = buf;
  while ((*buf = getchar()) != -1) buf++;
  *buf = 0; // Null-terminate the string

  printf("buf = %s\n", buf_start);
}
