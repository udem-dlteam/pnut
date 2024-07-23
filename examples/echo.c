/*
 * echo.c: Output the arguments passed to it
 *
 * Usage: ./echo.sh <args>
 */

void main(int argc, char **argv) {
  int i;
  for (i=1; i < argc; ++i) {
    printf("%s ", argv[i]);
  }
  printf("\n");
}
