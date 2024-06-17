/* typedef char *char_ptr; */

void putstring(char * s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int string_len(char * s) {
  int ix;
  ix = 0;
  while (s[ix] != 0) {
    ix = ix + 1;
  }
  return ix;
}

int string_sum(char * s) {
  int sum = 0;
  while (*s != '\0') {
    sum += *s;
    s = s + 1;
  }
  
  return sum;
}

int * iota_array(int start, int max) {
  int i = 0;
  int * arr;
  arr = malloc(max - start);
  while (i + start < max) {
    arr[i] = i + start;
    i = i + 1;
  }
  return arr;
}

int array_sum(int * arr, int len) {
    int sum;
    int i;
    sum = 0;
    i = 0;
    while (i < len) {
      sum += arr[i];
      i = i + 1;
    }
    return sum;
}

int main() {
  int n1;
  int n2;
  int n3;
  int arr;
  arr = iota_array(0, 50);
  n1 = string_len("12345");
  n2 = string_sum("Hello, world!");
  n3 = array_sum(arr, 50);
  putstring("n1 = ");
  putchar(n1 + 48);
  putchar('\n');
  putstring("n2 = ");
  if(n2 == 1161) {
    putstring("1161");
  } else {
    putstring("not 1161");
  }
  putchar('\n');
  putstring("n3 = ");
  if(n3 == 1225) {
    putstring("1225");
  } else {
    putstring("not 1225");
  }
  putchar('\n');
  return 0;
}
