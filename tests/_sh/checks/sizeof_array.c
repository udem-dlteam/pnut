// expect_comp_failure
typedef int arr[1000000];

void main() {
  int a = sizeof(arr);
}
