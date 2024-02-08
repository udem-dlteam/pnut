int main(int argc, char_ptr argv) {
  int i;
  printf("Number of arguments: %d\n", argc);
  printf("Arguments: ");

  for (i = 0; i < argc; i++) {
    printf("%s ", argv[i]);
  }

  printf("\n");
}
