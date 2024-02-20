/* args: abc def hij */
int main(int argc, char_ptr argv) {
  int i = 0;
  printf("Number of arguments: %d\n", argc);
  printf("Arguments:\n");
  while (i < argc) {
    printf("argv[%d] = %s\n", i, argv[i]);
    i++;
  }
}
