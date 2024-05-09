int main() {
  int_ptr arr;
  int acc;
  int i;

  arr = malloc(100);

  for(i = 0; i < 100; i++) {
    arr[i] = i;
  }

  acc = 0;
  for(i = 0; i < 100; i++) {
    acc += arr[i] * 13;
  }
  printf("acc: %d\n", acc);

  acc = 0;
  for(i = 0; i < 100; i++) {
    acc += (*(arr + i)) * 13;
  }
  printf("acc: %d\n", acc);
}
