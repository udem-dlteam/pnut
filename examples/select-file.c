/*
  This example shows how C code can be combined with shell utilities to create
  a simple program that lists files in the current directory and prints the
  contents of a selected file.
*/

#include "posix-utils.sh"

#define NULL 0

void print_array(char** arr) {
  int i = 0;
  while (arr[i] != NULL) {
    printf("%d: %s\n", i, arr[i]);
    i += 1;
  }
}

int array_len(char** arr) {
  int i = 0;
  while (arr[i] != NULL) { i += 1; }
  return i;
}

int read_int() {
  int n = 0;
  char c;
  while (1) {
    c = getchar();
    if (c >= '0' && c <= '9') {
      n = 10 * n + c - '0';
    } else {
      break;
    }
  }
  return n;
}

void main() {
  char **files;
  int ix;
  int len;
  printf("Files in current directory (%s)\n", pwd());
  files = ls();
  len = array_len(files);
  print_array(files); // Print files in current directory
  while (1) {
    printf("Select a file to print: ");
    ix = read_int();
    if (0 <= ix && ix < len) {
      break;
    }
    printf("Invalid index.\n");
  }
  cat(files[ix]);
}
