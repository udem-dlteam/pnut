void putstring(char * s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int main() {
  int * arr;
  int acc;

  arr = malloc(100);

  int i = 0;
  while(i < 100) {
    arr[i] = i;
    i = i + 1;
  }

  acc = 0;
  while(i < 100) {
    acc += arr[i] * 13;
    i = i + 1;
  }
  //printf("acc: %d\n", acc);
  putstring("acc: ");
  putchar(acc);
  putchar('\n');

  acc = 0;
  int i = 0;
  while (i < 100) {
    acc += (*(arr + i)) * 13;
    i = i + 1;
  }
  //printf("acc: %d\n", acc);
  putstring("acc: ");
  putchar(acc);
  putchar('\n');
  return 0;
}
