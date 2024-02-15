/* typedef char *char_ptr; */

int string_len(char_ptr s) {
  int ix;
  ix = 0;
  while (s[ix] != 0) {
    ix++;
  }
  return ix;
}

int string_sum(char_ptr s) {
  int sum;
  sum = 0;
  while (*s != 0) {
    sum += *s;
    s++;
  }
  return sum;
}

int iota_array(int start, int max) {
  int i;
  int_ptr arr;
  arr = malloc(max);
  for (i = 0; i + start < max; i++) {
    arr[i] = i + start;
   }
 return arr;
}

int array_sum(int_ptr arr, int len) {
    int sum;
    int i;
    sum = 0;
    i = 0;
    while (i < len) {
      sum += arr[i];
      i++;
    }
    return sum;
}

int main() {
  int n1;
  int n2;
  int n3;
  int arr;
  arr = iota_array(1, 50);
  n1 = string_len([1,2,3,4,5,0]);
  n2 = string_sum("Hello, world!");
  n3 = array_sum(arr, 50);
  printf("n1 = %d\n", n1);
  printf("n2 = %d\n", n2);
  printf("n3 = %d\n", n3);
}
