// File concatenate implementation
// #define INPUT_FILE "inputs/input.txt"

void main() {
  char ch;
  int *file = fopen(INPUT_FILE, 0);
  if (!file) {
      return;
  }


  while ((ch = fgetc(file)) != -1) {
      putchar(ch);
  }

  fclose(file);
}
