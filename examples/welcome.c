/*
 * welcome.c: Ask the user for their name and say hello.
 *
 * Usage: ./welcome.sh
 */

void main() {
  char* name = malloc(100); // Allocate 100 bytes for the name
  int i = 0;

  puts("What is your name?\n");
  while ((name[i] = getchar()) != -1 && name[i] != '\n') i += 1;
  name[i] = '\0';
  printf("Hello, %s\n", name);
}
