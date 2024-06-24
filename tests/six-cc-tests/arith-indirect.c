void putstring(char * s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int main() {
  int * arr;
  int acc;
  int i = 0;

  arr = malloc(100);

  i = 0;
  while(i < 100) {
    arr[i] = i;
    i = i + 1;
  }

  acc = 0;
  i = 0;
  while(i < 100) {
    acc += arr[i] * 13;
    i = i + 1;
  }

  putstring("acc: ");
  if(acc == 64350) {
    putstring("64350");
  } else {
    putstring("not 64350");
  }
  putchar(10);

  acc = 0;
  i = 0;
  while (i < 100) {
    acc += (*(arr + i)) * 13;
    i = i + 1;
  }
  putstring("acc: ");
  if(acc == 64350) {
    putstring("64350");
  } else {
    putstring("not 64350");
  }
  putchar(10);

  return 0;
}
