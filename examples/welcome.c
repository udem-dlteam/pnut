// ask the user to type their name

void main() {
  char name[100];
  int i = 0;

  puts("What is your name?\n");
  while ((name[i] = getchar()) != -1 && name[i] != '\n') i += 1;
  name[i] = '\0';
  printf("Hello, %s\n", name);
}
