#include_shell "posix-utils.sh"

#define NULL 0

void print_array(char** arr) {
  int i = 0;
  while (arr[i] != NULL) {
    printf("%s\n", arr[i]);
    i++;
  }
}

void main() {
  print_array(ls());
  printf("pwd: %s\n", pwd());
  cat("examples/shell-call.c", "examples/fib.c");
}
