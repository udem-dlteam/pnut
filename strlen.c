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
int main() {
  int n1;
  int n2;
  n1 = string_len([1,2,3,4,5,0]);
  n2 = string_sum("Hello, world!");
  exit(n2);
}
