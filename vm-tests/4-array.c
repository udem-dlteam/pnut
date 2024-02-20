// args:
int* iota_array(int n) {
  int* a;
  int i;
  a = malloc(n * sizeof(int));
  i = 0;
  while (i < n) {
    a[i] = i;
    i++;
  }
  return a;
}

int array_sum(int* a, int n) {
  int sum;
  int i;
  sum = 0;
  i = 0;
  while (i < n) {
    sum = sum + a[i];
    i++;
  }
  return sum;
}

int main(int argc, char **argv) {
  int ARR_SIZE;
  int* arr;
  ARR_SIZE = 10;
  arr = iota_array(ARR_SIZE);
  printf("%d\n", array_sum(arr, ARR_SIZE));
  return 0;
}
