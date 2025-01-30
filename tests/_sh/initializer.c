char str1[6]  = "abcde";
char str2[12] = "abcdef";
char str3[]   = "abcdef\t\t\t\t\t\t";

int arr1[5] = { 1, 2, 3, 'a', 'b' };
int arr2[15] = { 1, 2, 3, 'a', 'b' };

void main() {
  int i;

  for (i = 0; i < 6; i++) {
    printf("%c\n", str1[i]);
  }

  for (i = 0; i < 12; i++) {
    printf("%c\n", str2[i]);
  }

  for (i = 0; i < 13; i++) {
    printf("%c\n", str3[i]);
  }

  for (i = 0; i < 5; i++) {
    printf("%d\n", arr1[i]);
  }

  for (i = 0; i < 15; i++) {
    printf("%d\n", arr2[i]);
  }
}
